/*
    Title:  exporter.cpp - Export a function as an object or C file

    Copyright (c) 2006-7, 2015, 2016-20 David C.J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(_WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)

#else
#define ASSERT(x)
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#if (defined(_WIN32))
#include <tchar.h>
#else
#define _T(x) x
#define _tcslen strlen
#define _tcscmp strcmp
#define _tcscat strcat
#endif

#include "exporter.h"
#include "save_vec.h"
#include "polystring.h"
#include "run_time.h"
#include "osmem.h"
#include "scanaddrs.h"
#include "gc.h"
#include "machine_dep.h"
#include "diagnostics.h"
#include "memmgr.h"
#include "processes.h" // For IO_SPACING
#include "sys.h" // For EXC_Fail
#include "rtsentry.h"

#include "pexport.h"

#ifdef HAVE_PECOFF
#include "pecoffexport.h"
#elif defined(HAVE_ELF_H) || defined(HAVE_ELF_ABI_H)
#include "elfexport.h"
#elif defined(HAVE_MACH_O_RELOC_H)
#include "machoexport.h"
#endif

#if (defined(_WIN32))
#define NOMEMORY ERROR_NOT_ENOUGH_MEMORY
#define ERRORNUMBER _doserrno
#else
#define NOMEMORY ENOMEM
#define ERRORNUMBER errno
#endif

extern "C" {
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyExport(FirstArgument threadId, PolyWord fileName, PolyWord root);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyExportPortable(FirstArgument threadId, PolyWord fileName, PolyWord root);
}

/*
To export the function and everything reachable from it we need to copy
all the objects into a new area.  We leave tombstones in the original
objects by overwriting the length word.  That prevents us from copying an
object twice and breaks loops.  Once we've copied the objects we then
have to go back over the memory and turn the tombstones back into length
words.
*/

GraveYard::~GraveYard()
{
    free(graves);
}

// Used to calculate the space required for the ordinary mutables
// and the no-overwrite mutables.  They are interspersed in local space.
class MutSizes : public ScanAddress
{
public:
    MutSizes() : mutSize(0), noOverSize(0) {}

    virtual PolyObject *ScanObjectAddress(PolyObject *base) { return base; }// No Actually used

    virtual void ScanAddressesInObject(PolyObject *base, POLYUNSIGNED lengthWord)
    {
        const POLYUNSIGNED words = OBJ_OBJECT_LENGTH(lengthWord) + 1; // Include length word
        if (OBJ_IS_NO_OVERWRITE(lengthWord))
            noOverSize += words;
        else mutSize += words;
    }

    POLYUNSIGNED mutSize, noOverSize;
};

CopyScan::CopyScan(unsigned h/*=0*/): hierarchy(h)
{
    defaultImmSize = defaultMutSize = defaultCodeSize = defaultNoOverSize = 0;
    tombs = 0;
    graveYard = 0;
}

void CopyScan::initialise(bool isExport/*=true*/)
{
    ASSERT(gMem.eSpaces.size() == 0);
    // Set the space sizes to a proportion of the space currently in use.
    // Computing these sizes is not obvious because CopyScan is used both
    // for export and for saved states.  For saved states in particular we
    // want to use a smaller size because they are retained after we save
    // the state and if we have many child saved states it's important not
    // to waste memory.
    if (hierarchy == 0)
    {
        graveYard = new GraveYard[gMem.pSpaces.size()];
        if (graveYard == 0)
        {
            if (debugOptions & DEBUG_SAVING)
                Log("SAVE: Unable to allocate graveyard, size: %lu.\n", gMem.pSpaces.size());
            throw MemoryException();
        }
    }

    for (std::vector<PermanentMemSpace*>::iterator i = gMem.pSpaces.begin(); i < gMem.pSpaces.end(); i++)
    {
        PermanentMemSpace *space = *i;
        if (space->hierarchy >= hierarchy) {
            // Include this if we're exporting (hierarchy=0) or if we're saving a state
            // and will include this in the new state.
            size_t size = (space->top-space->bottom)/4;
            if (space->noOverwrite)
                defaultNoOverSize += size;
            else if (space->isMutable)
                defaultMutSize += size;
            else if (space->isCode)
                defaultCodeSize += size;
            else
                defaultImmSize += size;
            if (space->hierarchy == 0 && ! space->isMutable)
            {
                // We need a separate area for the tombstones because this is read-only
                graveYard[tombs].graves = (PolyWord*)calloc(space->spaceSize(), sizeof(PolyWord));
                if (graveYard[tombs].graves == 0)
                {
                    if (debugOptions & DEBUG_SAVING)
                        Log("SAVE: Unable to allocate graveyard for permanent space, size: %lu.\n",
                            space->spaceSize() * sizeof(PolyWord));
                    throw MemoryException();
                }
                if (debugOptions & DEBUG_SAVING)
                    Log("SAVE: Allocated graveyard for permanent space, %p size: %lu.\n",
                        graveYard[tombs].graves, space->spaceSize() * sizeof(PolyWord));
                graveYard[tombs].startAddr = space->bottom;
                graveYard[tombs].endAddr = space->top;
                tombs++;
            }
        }
    }
    for (std::vector<LocalMemSpace*>::iterator i = gMem.lSpaces.begin(); i < gMem.lSpaces.end(); i++)
    {
        LocalMemSpace *space = *i;
        uintptr_t size = space->allocatedSpace();
        // It looks as though the mutable size generally gets
        // overestimated while the immutable size is correct.
        if (space->isMutable)
        {
            MutSizes sizeMut;
            sizeMut.ScanAddressesInRegion(space->bottom, space->lowerAllocPtr);
            sizeMut.ScanAddressesInRegion(space->upperAllocPtr, space->top);
            defaultNoOverSize += sizeMut.noOverSize / 4;
            defaultMutSize += sizeMut.mutSize / 4;
        }
        else
            defaultImmSize += size/2;
    }
    for (std::vector<CodeSpace *>::iterator i = gMem.cSpaces.begin(); i < gMem.cSpaces.end(); i++)
    {
        CodeSpace *space = *i;
        uintptr_t size = space->spaceSize();
        defaultCodeSize += size/2;
    }
    if (isExport)
    {
        // Minimum 1M words.
        if (defaultMutSize < 1024*1024) defaultMutSize = 1024*1024;
        if (defaultImmSize < 1024*1024) defaultImmSize = 1024*1024;
        if (defaultCodeSize < 1024*1024) defaultCodeSize = 1024*1024;
#ifdef MACOSX
        // Limit the segment size for Mac OS X.  The linker has a limit of 2^24 relocations
        // in a segment so this is a crude way of ensuring the limit isn't exceeded.
        // It's unlikely to be exceeded by the code itself.
        // Actually, from trial-and-error, the limit seems to be around 6M.
        if (defaultMutSize > 6 * 1024 * 1024) defaultMutSize = 6 * 1024 * 1024;
        if (defaultImmSize > 6 * 1024 * 1024) defaultImmSize = 6 * 1024 * 1024;
#endif
        if (defaultNoOverSize < 4096) defaultNoOverSize = 4096; // Except for the no-overwrite area
    }
    else
    {
        // Much smaller minimum sizes for saved states.
        if (defaultMutSize < 1024) defaultMutSize = 1024;
        if (defaultImmSize < 4096) defaultImmSize = 4096;
        if (defaultCodeSize < 4096) defaultCodeSize = 4096;
        if (defaultNoOverSize < 4096) defaultNoOverSize = 4096;
        // Set maximum sizes as well.  We may have insufficient contiguous space for
        // very large areas.
        if (defaultMutSize > 1024 * 1024) defaultMutSize = 1024 * 1024;
        if (defaultImmSize > 1024 * 1024) defaultImmSize = 1024 * 1024;
        if (defaultCodeSize > 1024 * 1024) defaultCodeSize = 1024 * 1024;
        if (defaultNoOverSize > 1024 * 1024) defaultNoOverSize = 1024 * 1024;
    }
    if (debugOptions & DEBUG_SAVING)
        Log("SAVE: Copyscan default sizes: Immutable: %" POLYUFMT ", Mutable: %" POLYUFMT ", Code: %" POLYUFMT ", No-overwrite %" POLYUFMT ".\n",
            defaultImmSize, defaultMutSize, defaultCodeSize, defaultNoOverSize);
}

CopyScan::~CopyScan()
{
    gMem.DeleteExportSpaces();
    if (graveYard)
        delete[](graveYard);
}


// This function is called for each address in an object
// once it has been copied to its new location.  We copy first
// then scan to update the addresses.
POLYUNSIGNED CopyScan::ScanAddressAt(PolyWord *pt)
{
    PolyWord val = *pt;
    // Ignore integers.
    if (IS_INT(val) || val == PolyWord::FromUnsigned(0))
        return 0;

    PolyObject *obj = val.AsObjPtr();
    POLYUNSIGNED l = ScanAddress(&obj);
    *pt = obj;
    return l;
}

// This function is called for each address in an object
// once it has been copied to its new location.  We copy first
// then scan to update the addresses.
POLYUNSIGNED CopyScan::ScanAddress(PolyObject **pt)
{
    PolyObject *obj = *pt;
    MemSpace *space = gMem.SpaceForObjectAddress(obj);
    ASSERT(space != 0);
    // We may sometimes get addresses that have already been updated
    // to point to the new area.  e.g. (only?) in the case of constants
    // that have been updated in ScanConstantsWithinCode.
    if (space->spaceType == ST_EXPORT)
        return 0;

    // If this is at a lower level than the hierarchy we are saving
    // then leave it untouched.
    if (space->spaceType == ST_PERMANENT)
    {
        PermanentMemSpace *pmSpace = (PermanentMemSpace*)space;
        if (pmSpace->hierarchy < hierarchy)
            return 0;
    }

    // Have we already scanned this?
    if (obj->ContainsForwardingPtr())
    {
        // Update the address to the new value.
#ifdef POLYML32IN64
        PolyObject *newAddr;
        if (space->isCode)
            newAddr = (PolyObject*)(globalCodeBase + ((obj->LengthWord() & ~_OBJ_TOMBSTONE_BIT) << 1));
        else newAddr = obj->GetForwardingPtr();
#else
        PolyObject *newAddr = obj->GetForwardingPtr();
#endif
        *pt = newAddr;
        return 0; // No need to scan it again.
    }
    else if (space->spaceType == ST_PERMANENT)
    {
        // See if we have this in the grave-yard.
        for (unsigned i = 0; i < tombs; i++)
        {
            GraveYard *g = &graveYard[i];
            if ((PolyWord*)obj >= g->startAddr && (PolyWord*)obj < g->endAddr)
            {
                PolyWord *tombAddr = g->graves + ((PolyWord*)obj - g->startAddr);
                PolyObject *tombObject = (PolyObject*)tombAddr;
                if (tombObject->ContainsForwardingPtr())
                {
#ifdef POLYML32IN64
                    PolyObject *newAddr;
                    if (space->isCode)
                        newAddr = (PolyObject*)(globalCodeBase + ((tombObject->LengthWord() & ~_OBJ_TOMBSTONE_BIT) << 1));
                    else newAddr = tombObject->GetForwardingPtr();
#else
                    PolyObject *newAddr = tombObject->GetForwardingPtr();
#endif
                    *pt = newAddr;
                    return 0;
                }
                break; // No need to look further
            }
        }
    }

    // No, we need to copy it.
    ASSERT(space->spaceType == ST_LOCAL || space->spaceType == ST_PERMANENT ||
        space->spaceType == ST_CODE);
    POLYUNSIGNED lengthWord = obj->LengthWord();
    POLYUNSIGNED words = OBJ_OBJECT_LENGTH(lengthWord);

    PolyObject *newObj = 0;
    PolyObject* writAble = 0;
    bool isMutableObj = obj->IsMutable();
    bool isNoOverwrite = false;
    bool isByteObj = obj->IsByteObject();
    bool isCodeObj = false;
    if (isMutableObj)
        isNoOverwrite = obj->IsNoOverwriteObject();
    else isCodeObj = obj->IsCodeObject();
    // Allocate a new address for the object.
    for (std::vector<PermanentMemSpace *>::iterator i = gMem.eSpaces.begin(); i < gMem.eSpaces.end(); i++)
    {
        PermanentMemSpace *space = *i;
        if (isMutableObj == space->isMutable &&
            isNoOverwrite == space->noOverwrite &&
            isByteObj == space->byteOnly &&
            isCodeObj == space->isCode)
        {
            ASSERT(space->topPointer <= space->top && space->topPointer >= space->bottom);
            size_t spaceLeft = space->top - space->topPointer;
            if (spaceLeft > words)
            {
                newObj = (PolyObject*)(space->topPointer + 1);
                writAble = space->writeAble(newObj);
                space->topPointer += words + 1;
#ifdef POLYML32IN64
                // Maintain the odd-word alignment of topPointer
                if ((words & 1) == 0 && space->topPointer < space->top)
                {
                    *space->writeAble(space->topPointer) = PolyWord::FromUnsigned(0);
                    space->topPointer++;
                }
#endif
                break;
            }
        }
    }
    if (newObj == 0)
    {
        // Didn't find room in the existing spaces.  Create a new space.
        uintptr_t spaceWords;
        if (isMutableObj)
        {
            if (isNoOverwrite) spaceWords = defaultNoOverSize;
            else spaceWords = defaultMutSize;
        }
        else
        {
            if (isCodeObj) spaceWords = defaultCodeSize;
            else spaceWords = defaultImmSize;
        }
        if (spaceWords <= words)
            spaceWords = words + 1; // Make sure there's space for this object.
        PermanentMemSpace *space = gMem.NewExportSpace(spaceWords, isMutableObj, isNoOverwrite, isCodeObj);
        if (isByteObj) space->byteOnly = true;
        if (space == 0)
        {
            if (debugOptions & DEBUG_SAVING)
                Log("SAVE: Unable to allocate export space, size: %lu.\n", spaceWords);
            // Unable to allocate this.
            throw MemoryException();
        }
        newObj = (PolyObject*)(space->topPointer + 1);
        writAble = space->writeAble(newObj);
        space->topPointer += words + 1;
#ifdef POLYML32IN64
        // Maintain the odd-word alignment of topPointer
        if ((words & 1) == 0 && space->topPointer < space->top)
        {
            *space->writeAble(space->topPointer) = PolyWord::FromUnsigned(0);
            space->topPointer++;
        }
#endif
        ASSERT(space->topPointer <= space->top && space->topPointer >= space->bottom);
    }

    writAble->SetLengthWord(lengthWord); // copy length word

    if (hierarchy == 0 /* Exporting object module */ && isNoOverwrite && isMutableObj && !isByteObj)
    {
        // These are not exported. They are used for special values e.g. mutexes
        // that should be set to 0/nil/NONE at start-up.
        // Weak+No-overwrite byte objects are used for entry points and volatiles
        // in the foreign-function interface and have to be treated specially.

        // Note: this must not be done when exporting a saved state because the
        // copied version is used as the local data for the rest of the session.
        for (POLYUNSIGNED i = 0; i < words; i++)
            writAble->Set(i, TAGGED(0));
    }
    else memcpy(writAble, obj, words * sizeof(PolyWord));

    if (space->spaceType == ST_PERMANENT && !space->isMutable && ((PermanentMemSpace*)space)->hierarchy == 0)
    {
        // The immutable permanent areas are read-only.
        unsigned m;
        for (m = 0; m < tombs; m++)
        {
            GraveYard *g = &graveYard[m];
            if ((PolyWord*)obj >= g->startAddr && (PolyWord*)obj < g->endAddr)
            {
                PolyWord *tombAddr = g->graves + ((PolyWord*)obj - g->startAddr);
                PolyObject *tombObject = (PolyObject*)tombAddr;
#ifdef POLYML32IN64
                if (isCodeObj)
                {
                    POLYUNSIGNED ll = (POLYUNSIGNED)(((PolyWord*)newObj - globalCodeBase) >> 1 | _OBJ_TOMBSTONE_BIT);
                    tombObject->SetLengthWord(ll);
                }
                else tombObject->SetForwardingPtr(newObj);
#else
                tombObject->SetForwardingPtr(newObj);
#endif
                break; // No need to look further
            }
        }
        ASSERT(m < tombs); // Should be there.
    }
    else if (isCodeObj)
#ifdef POLYML32IN64
    // If this is a code address we can't use the usual forwarding pointer format.
    // Instead we have to compute the offset relative to the base of the code.
    {
        POLYUNSIGNED ll = (POLYUNSIGNED)(((PolyWord*)newObj-globalCodeBase) >> 1 | _OBJ_TOMBSTONE_BIT);
        gMem.SpaceForObjectAddress(obj)->writeAble(obj)->SetLengthWord(ll);
    }
#else
        gMem.SpaceForObjectAddress(obj)->writeAble(obj)->SetForwardingPtr(newObj);
#endif
    else obj->SetForwardingPtr(newObj); // Put forwarding pointer in old object.

    if (OBJ_IS_CODE_OBJECT(lengthWord))
    {
        // We don't need to worry about flushing the instruction cache
        // since we're not going to execute this code here.
        // We do have to update any relative addresses within the code
        // to take account of its new position.  We have to do that now
        // even though ScanAddressesInObject will do it again because this
        // is the only point where we have both the old and the new addresses.
        machineDependent->ScanConstantsWithinCode(newObj, obj, words, this);
    }
    *pt = newObj; // Update it to the newly copied object.
    return lengthWord;  // This new object needs to be scanned.
}

// The address of code in the code area.  We treat this as a normal heap cell.
// We will probably need to copy this and to process addresses within it.
POLYUNSIGNED CopyScan::ScanCodeAddressAt(PolyObject **pt)
{
    POLYUNSIGNED lengthWord = ScanAddress(pt);
    if (lengthWord)
        ScanAddressesInObject(*pt, lengthWord);
    return 0;
}

PolyObject *CopyScan::ScanObjectAddress(PolyObject *base)
{
    PolyWord val = base;
    // Scan this as an address. 
    POLYUNSIGNED lengthWord = CopyScan::ScanAddressAt(&val);
    if (lengthWord)
        ScanAddressesInObject(val.AsObjPtr(), lengthWord);
    return val.AsObjPtr();
}

#define MAX_EXTENSION   4 // The longest extension we may need to add is ".obj"

// Convert the forwarding pointers in a region back into length words.

// Generally if this object has a forwarding pointer that's
// because we've moved it into the export region.  We can,
// though, get multiple levels of forwarding if there is an object
// that has been shifted up by a garbage collection, leaving a forwarding
// pointer and then that object has been moved to the export region.
// We mustn't turn locally forwarded values back into ordinary objects
// because they could contain addresses that are no longer valid.
static POLYUNSIGNED GetObjLength(PolyObject *obj)
{
    if (obj->ContainsForwardingPtr())
    {
        PolyObject *forwardedTo;
#ifdef POLYML32IN64
        {
            MemSpace *space = gMem.SpaceForObjectAddress(obj);
            if (space->isCode)
                forwardedTo = (PolyObject*)(globalCodeBase + ((obj->LengthWord() & ~_OBJ_TOMBSTONE_BIT) << 1));
            else forwardedTo = obj->GetForwardingPtr();
        }
#else
        forwardedTo = obj->GetForwardingPtr();
#endif
        POLYUNSIGNED length = GetObjLength(forwardedTo);
        MemSpace *space = gMem.SpaceForObjectAddress(forwardedTo);
        if (space->spaceType == ST_EXPORT)
            gMem.SpaceForObjectAddress(obj)->writeAble(obj)->SetLengthWord(length);
        return length;
    }
    else {
        ASSERT(obj->ContainsNormalLengthWord());
        return obj->LengthWord();
    }
}

static void FixForwarding(PolyWord *pt, size_t space)
{
    while (space)
    {
        pt++;
        PolyObject *obj = (PolyObject*)pt;
#ifdef POLYML32IN64
        if ((uintptr_t)obj & 4)
        {
            // Skip filler words needed to align to an even word
            space--;
            continue; // We've added 1 to pt so just loop.
        }
#endif
        size_t length = OBJ_OBJECT_LENGTH(GetObjLength(obj));
        pt += length;
        ASSERT(space > length);
        space -= length+1;
    }
}

class ExportRequest: public MainThreadRequest
{
public:
    ExportRequest(Handle root, Exporter *exp): MainThreadRequest(MTP_EXPORTING),
        exportRoot(root), exporter(exp) {}

    virtual void Perform() { exporter->RunExport(exportRoot->WordP()); }
    Handle exportRoot;
    Exporter *exporter;
};

static void exporter(TaskData *taskData, Handle fileName, Handle root, const TCHAR *extension, Exporter *exports)
{
    size_t extLen = _tcslen(extension);
    TempString fileNameBuff(Poly_string_to_T_alloc(fileName->Word(), extLen));
    if (fileNameBuff == NULL)
        raise_syscall(taskData, "Insufficient memory", NOMEMORY);
    size_t length = _tcslen(fileNameBuff);

    // Does it already have the extension?  If not add it on.
    if (length < extLen || _tcscmp(fileNameBuff + length - extLen, extension) != 0)
        _tcscat(fileNameBuff, extension);
#if (defined(_WIN32) && defined(UNICODE))
    exports->exportFile = _wfopen(fileNameBuff, L"wb");
#else
    exports->exportFile = fopen(fileNameBuff, "wb");
#endif
    if (exports->exportFile == NULL)
        raise_syscall(taskData, "Cannot open export file", ERRORNUMBER);

    // Request a full GC  to reduce the size of fix-ups.
    FullGC(taskData);
    // Request the main thread to do the export.
    ExportRequest request(root, exports);
    processes->MakeRootRequest(taskData, &request);
    if (exports->errorMessage)
        raise_fail(taskData, exports->errorMessage);
}

// This is called by the initial thread to actually do the export.
void Exporter::RunExport(PolyObject *rootFunction)
{
    Exporter *exports = this;

    PolyObject *copiedRoot = 0;
    CopyScan copyScan(hierarchy);

    try {
        copyScan.initialise();
        // Copy the root and everything reachable from it into the temporary area.
        copiedRoot = copyScan.ScanObjectAddress(rootFunction);
    }
    catch (MemoryException &)
    {
        // If we ran out of memory.
        copiedRoot = 0;
    }

    // Fix the forwarding pointers.
    for (std::vector<LocalMemSpace*>::iterator i = gMem.lSpaces.begin(); i < gMem.lSpaces.end(); i++)
    {
        LocalMemSpace *space = *i;
        // Local areas only have objects from the allocation pointer to the top.
        FixForwarding(space->bottom, space->lowerAllocPtr - space->bottom);
        FixForwarding(space->upperAllocPtr, space->top - space->upperAllocPtr);
    }
    for (std::vector<PermanentMemSpace*>::iterator i = gMem.pSpaces.begin(); i < gMem.pSpaces.end(); i++)
    {
        MemSpace *space = *i;
        // Permanent areas are filled with objects from the bottom.
        FixForwarding(space->bottom, space->top - space->bottom);
    }
    for (std::vector<CodeSpace *>::iterator i = gMem.cSpaces.begin(); i < gMem.cSpaces.end(); i++)
    {
        MemSpace *space = *i;
        // Code areas are filled with objects from the bottom.
        FixForwarding(space->bottom, space->top - space->bottom);
    }

    // Reraise the exception after cleaning up the forwarding pointers.
    if (copiedRoot == 0)
    {
        exports->errorMessage = "Insufficient Memory";
        return;
    }

    // Copy the areas into the export object.
    size_t tableEntries = gMem.eSpaces.size();
    unsigned memEntry = 0;
    if (hierarchy != 0) tableEntries += gMem.pSpaces.size();
    exports->memTable = new memoryTableEntry[tableEntries];

    // If we're constructing a module we need to include the global spaces.
    if (hierarchy != 0)
    {
        // Permanent spaces from the executable.
        for (std::vector<PermanentMemSpace*>::iterator i = gMem.pSpaces.begin(); i < gMem.pSpaces.end(); i++)
        {
            PermanentMemSpace *space = *i;
            if (space->hierarchy < hierarchy)
            {
                memoryTableEntry *entry = &exports->memTable[memEntry++];
                entry->mtOriginalAddr = entry->mtCurrentAddr = space->bottom;
                entry->mtLength = (space->topPointer-space->bottom)*sizeof(PolyWord);
                entry->mtIndex = space->index;
                entry->mtFlags = 0;
                if (space->isMutable) entry->mtFlags |= MTF_WRITEABLE;
                if (space->isCode) entry->mtFlags |= MTF_EXECUTABLE;
            }
        }
        newAreas = memEntry;
    }

    for (std::vector<PermanentMemSpace *>::iterator i = gMem.eSpaces.begin(); i < gMem.eSpaces.end(); i++)
    {
        memoryTableEntry *entry = &exports->memTable[memEntry++];
        PermanentMemSpace *space = *i;
        entry->mtOriginalAddr = entry->mtCurrentAddr = space->bottom;
        entry->mtLength = (space->topPointer-space->bottom)*sizeof(PolyWord);
        entry->mtIndex = hierarchy == 0 ? memEntry-1 : space->index;
        entry->mtFlags = 0;
        if (space->isMutable)
        {
            entry->mtFlags = MTF_WRITEABLE;
            if (space->noOverwrite) entry->mtFlags |= MTF_NO_OVERWRITE;
        }
        if (space->isCode) entry->mtFlags |= MTF_EXECUTABLE;
        if (space->byteOnly) entry->mtFlags |= MTF_BYTES;
    }

    ASSERT(memEntry == tableEntries);
    exports->memTableEntries = memEntry;
    exports->rootFunction = copiedRoot;
    try {
        // This can raise MemoryException at least in PExport::exportStore. 
        exports->exportStore();
    }
    catch (MemoryException &) {
        exports->errorMessage = "Insufficient Memory";
    }
}

// Functions called via the RTS call.
Handle exportNative(TaskData *taskData, Handle args)
{
#ifdef HAVE_PECOFF
    // Windows including Cygwin
#if (defined(_WIN32))
    const TCHAR *extension = _T(".obj"); // Windows
#else
    const char *extension = ".o"; // Cygwin
#endif
    PECOFFExport exports;
    exporter(taskData, taskData->saveVec.push(args->WordP()->Get(0)),
        taskData->saveVec.push(args->WordP()->Get(1)), extension, &exports);
#elif defined(HAVE_ELF_H) || defined(HAVE_ELF_ABI_H)
    // Most Unix including Linux, FreeBSD and Solaris.
    const char *extension = ".o";
    ELFExport exports;
    exporter(taskData, taskData->saveVec.push(args->WordP()->Get(0)),
        taskData->saveVec.push(args->WordP()->Get(1)), extension, &exports);
#elif defined(HAVE_MACH_O_RELOC_H)
    // Mac OS-X
    const char *extension = ".o";
    MachoExport exports;
    exporter(taskData, taskData->saveVec.push(args->WordP()->Get(0)),
        taskData->saveVec.push(args->WordP()->Get(1)), extension, &exports);
#else
    raise_exception_string (taskData, EXC_Fail, "Native export not available for this platform");
#endif
    return taskData->saveVec.push(TAGGED(0));
}

Handle exportPortable(TaskData *taskData, Handle args)
{
    PExport exports;
    exporter(taskData, taskData->saveVec.push(args->WordP()->Get(0)),
        taskData->saveVec.push(args->WordP()->Get(1)), _T(".txt"), &exports);
    return taskData->saveVec.push(TAGGED(0));
}

POLYUNSIGNED PolyExport(FirstArgument threadId, PolyWord fileName, PolyWord root)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedName = taskData->saveVec.push(fileName);
    Handle pushedRoot = taskData->saveVec.push(root);

    try {
#ifdef HAVE_PECOFF
        // Windows including Cygwin
#if (defined(_WIN32))
        const TCHAR *extension = _T(".obj"); // Windows
#else
        const char *extension = ".o"; // Cygwin
#endif
        PECOFFExport exports;
        exporter(taskData, pushedName, pushedRoot, extension, &exports);
#elif defined(HAVE_ELF_H) || defined(HAVE_ELF_ABI_H)
        // Most Unix including Linux, FreeBSD and Solaris.
        const char *extension = ".o";
        ELFExport exports;
        exporter(taskData, pushedName, pushedRoot, extension, &exports);
#elif defined(HAVE_MACH_O_RELOC_H)
        // Mac OS-X
        const char *extension = ".o";
        MachoExport exports;
        exporter(taskData, pushedName, pushedRoot, extension, &exports);
#else
        raise_exception_string (taskData, EXC_Fail, "Native export not available for this platform");
#endif
    } catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned(); // Returns unit
}

POLYUNSIGNED PolyExportPortable(FirstArgument threadId, PolyWord fileName, PolyWord root)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedName = taskData->saveVec.push(fileName);
    Handle pushedRoot = taskData->saveVec.push(root);

    try {
        PExport exports;
        exporter(taskData, pushedName, pushedRoot, _T(".txt"), &exports);
    } catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned(); // Returns unit
}


// Helper functions for exporting.  We need to produce relocation information
// and this code is common to every method.
Exporter::Exporter(unsigned int h): exportFile(NULL), errorMessage(0), hierarchy(h), memTable(0), newAreas(0)
{
}

Exporter::~Exporter()
{
    delete[](memTable);
    if (exportFile)
        fclose(exportFile);
}

void Exporter::relocateValue(PolyWord *pt)
{
#ifndef POLYML32IN64
    PolyWord q = *pt;
    if (IS_INT(q) || q == PolyWord::FromUnsigned(0)) {}
    else createRelocation(pt);
#endif
}

void Exporter::createRelocation(PolyWord* pt)
{
    *gMem.SpaceForAddress(pt)->writeAble(pt) = createRelocation(*pt, pt);
}

// Check through the areas to see where the address is.  It must be
// in one of them.
unsigned Exporter::findArea(void *p)
{
    for (unsigned i = 0; i < memTableEntries; i++)
    {
        if (p > memTable[i].mtOriginalAddr &&
            p <= (char*)memTable[i].mtOriginalAddr + memTable[i].mtLength)
            return i;
    }
    { ASSERT(0); }
    return 0;
}

void Exporter::relocateObject(PolyObject *p)
{
    if (p->IsByteObject())
    {
        if (p->IsMutable() && p->IsWeakRefObject())
        {
            // Weak mutable byte refs are used for external references and
            // also in the FFI for non-persistent values.
            bool isFuncPtr = true;
            const char *entryName = getEntryPointName(p, &isFuncPtr);
            if (entryName != 0) addExternalReference(p, entryName, isFuncPtr);
            // Clear the first word of the data.
            ASSERT(p->Length() >= sizeof(uintptr_t)/sizeof(PolyWord));
            *(uintptr_t*)p = 0;
        }
    }
    else if (p->IsCodeObject())
    {
        POLYUNSIGNED constCount;
        PolyWord *cp;
        ASSERT(! p->IsMutable() );
        p->GetConstSegmentForCode(cp, constCount);
        /* Now the constants. */
        for (POLYUNSIGNED i = 0; i < constCount; i++) relocateValue(&(cp[i]));

    }
    else // Closure and ordinary objects
    {
        POLYUNSIGNED length = p->Length();
        for (POLYUNSIGNED i = 0; i < length; i++) relocateValue(p->Offset(i));
    }
}

ExportStringTable::ExportStringTable(): strings(0), stringSize(0), stringAvailable(0)
{
}

ExportStringTable::~ExportStringTable()
{
    free(strings);
}

// Add a string to the string table, growing it if necessary.
unsigned long ExportStringTable::makeEntry(const char *str)
{
    unsigned len = (unsigned)strlen(str);
    unsigned long entry = stringSize;
    if (stringSize + len + 1 > stringAvailable)
    {
        stringAvailable = stringAvailable+stringAvailable/2;
        if (stringAvailable < stringSize + len + 1)
            stringAvailable = stringSize + len + 1 + 500;
        char* newStrings = (char*)realloc(strings, stringAvailable);
        if (newStrings == 0)
        {
            if (debugOptions & DEBUG_SAVING)
                Log("SAVE: Unable to realloc string table, size: %lu.\n", stringAvailable);
            throw MemoryException();
        }
        else strings = newStrings;
     }
    strcpy(strings + stringSize, str);
    stringSize += len + 1;
    return entry;
}

struct _entrypts exporterEPT[] =
{
    { "PolyExport",                     (polyRTSFunction)&PolyExport},
    { "PolyExportPortable",             (polyRTSFunction)&PolyExportPortable},

    { NULL, NULL} // End of list.
};
