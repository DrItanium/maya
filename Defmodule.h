#ifndef _H_moduldef
#define _H_moduldef
#include <memory>
#include <map>
#include <string>
#include <list>
#include "HoldsEnvironmentCallback.h"
#include "Construct.h"
namespace maya {
    class Defmodule;
    class DefmoduleItemHeader : public HoldsEnvironmentCallback {
    public:
        using Self = DefmoduleItemHeader;
        using Ptr = std::shared_ptr<Self>;
        using ItemList = std::list<Construct::Ptr>;
        using iterator = ItemList::iterator;
        using const_iterator = ItemList::const_iterator ;
    public:
        DefmoduleItemHeader(Environment& env, std::shared_ptr<Defmodule> parent);
        virtual ~DefmoduleItemHeader() = default;
        auto getParent() const noexcept { return _parent; }
        void add(Construct::Ptr item);
        iterator begin() noexcept { return _items.begin(); }
        const_iterator begin() const noexcept { return _items.begin(); }
        iterator end() noexcept { return _items.end(); }
        const_iterator end() const noexcept { return _items.end(); }
        auto size() const noexcept { return _items.size(); }
    private:
        std::shared_ptr<Defmodule> _parent;
        std::list<Construct::Ptr> _items;
    };
    class ModuleItem : public HoldsEnvironmentCallback {
    public:
        using Self = ModuleItem;
        using Ptr = std::shared_ptr<Self>;
    public:
        ModuleItem(Environment& parent, const std::string& name, uint32_t index);
        virtual ~ModuleItem() = default;
        /// @todo how to support the allocate function?
        /// @todo figure out if freeFunction is necessary
        /// @todo implement bloadModuleReference once binary load/save support is brought back in
        virtual std::shared_ptr<Construct> find(const std::string& name) = 0;
    private:
        std::string _name;
        uint32_t _index = 0;
        bool _exportable = false;
    };

    class PortItem : public HoldsEnvironmentCallback {
    public:
        using Self = PortItem;
        using Ptr = std::shared_ptr<Self>;
        using PtrList = std::list<Ptr>;
    public:
        PortItem(Environment& parent, Lexeme::Ptr moduleName, Lexeme::Ptr constructType, Lexeme::Ptr constructName);
        auto getModuleName() const noexcept { return _moduleName; }
        auto getConstructType() const noexcept { return _constructType; }
        auto getConstructName() const noexcept { return _constructName; }
    private:
        Lexeme::Ptr _moduleName;
        Lexeme::Ptr _constructType;
        Lexeme::Ptr _constructName;
    };
    /**
     * @brief A Named container of constructs which acts like a C++ namespace
     */
    class Defmodule : public Construct {
    public:
        using Self = Defmodule;
        using Ptr = std::shared_ptr<Self>;
    public:
        Defmodule(Environment& parent, const std::string& name, const std::string& ppForm);
        void setVisited(bool value) noexcept { _visited = value; }
        constexpr auto visited() const noexcept { return _visited; }
    private:
        std::multimap<Construct::Type, Construct::PtrList> _items;
        PortItem::PtrList _imports, _exports;
        bool _visited = false;
    };

    class ModuleStackItem {
    public:
        using Self = ModuleStackItem;
        using Ptr = std::shared_ptr<Self>;
        using PtrList = std::list<Ptr>;
    public:
        ModuleStackItem(Defmodule::Ptr targetModule, bool change) : _changeFlag(change), _theModule(targetModule) { }
        constexpr auto getChangeFlag() const noexcept { return _changeFlag; }
        void setChangeFlag(bool v) noexcept { _changeFlag = v; }
        auto getModule() const noexcept { return _theModule; }
    private:
        bool _changeFlag = false;
        Defmodule::Ptr _theModule;
    };
} // end namespace maya
#if 0

typedef struct defmodule Defmodule;
typedef struct portItem PortItem;
typedef struct defmoduleItemHeader DefmoduleItemHeader;
typedef struct moduleItem ModuleItem;
typedef struct moduleStackItem ModuleStackItem;

typedef void *AllocateModuleFunction(const Environment::Ptr&);
typedef void FreeModuleFunction(const Environment::Ptr&, void *);

enum class ConstructType {
    DEFMODULE,
    DEFRULE,
    DEFTEMPLATE,
    DEFFACTS,
    DEFGLOBAL,
    DEFFUNCTION,
    DEFGENERIC,
    DEFMETHOD,
    DEFCLASS,
    DEFMESSAGE_HANDLER,
    DEFINSTANCES
};

#include <cstdio>
#include "PatternEntity.hxx"
#include "UserData.h"
#include "Utility.h"

struct ConstructHeader {
public:
    using Self = ConstructHeader;
    using Ptr = std::shared_ptr<Self>;
public:
    ConstructType constructType;
    CLIPSLexeme::Ptr name;
    std::string ppForm;
    std::shared_ptr<DefmoduleItemHeader> whichModule;
    unsigned long bsaveID = 0;
    Ptr next;
    struct userData *usrData;
    Environment env;
};

struct defmoduleItemHeader {
    std::shared_ptr<Defmodule> theModule;
    ConstructHeader::Ptr firstItem;
    ConstructHeader::Ptr lastItem;
};

typedef ConstructHeader *FindConstructFunction(const Environment::Ptr&, const char *);
typedef ConstructHeader *GetNextConstructFunction(const Environment::Ptr&, ConstructHeader *);
typedef bool IsConstructDeletableFunction(ConstructHeader *);
typedef bool DeleteConstructFunction(ConstructHeader *, const Environment::Ptr&);
typedef void FreeConstructFunction(const Environment::Ptr&, ConstructHeader *);

/**********************************************************************/
/* defmodule                                                          */
/* ----------                                                         */
/* name: The name of the defmodule (stored as a reference in the      */
/*   table).                                                          */
/*                                                                    */
/* ppForm: The pretty print representation of the defmodule (used by  */
/*   the save and ppdefmodule commands).                              */
/*                                                                    */
/* itemsArray: An array of pointers to the module specific data used  */
/*   by each construct specified with the RegisterModuleItem          */
/*   function. The data pointer stored in the array is allocated by   */
/*   the allocateFunction in moduleItem data structure.               */
/*                                                                    */
/* importList: The list of items which are being imported by this     */
/*   module from other modules.                                       */
/*                                                                    */
/* next: A pointer to the next defmodule data structure.              */
/**********************************************************************/

struct defmodule {
public:
    using Self = defmodule;
    using Ptr = std::shared_ptr<Self>;
public:
    ConstructHeader header;
    DefmoduleItemHeader **itemsArray;
    PortItem *importList;
    PortItem *exportList;
    bool visitedFlag;
};

struct portItem {
public:
    using Self = defmodule;
    using Ptr = std::shared_ptr<Self>;
public:
    CLIPSLexeme::Ptr moduleName;
    CLIPSLexeme::Ptr constructType;
    CLIPSLexeme::Ptr constructName;
    Ptr next;
};

#define MIHS (DefmoduleItemHeader *)

/**********************************************************************/
/* moduleItem                                                         */
/* ----------                                                         */
/* name: The name of the construct which can be placed in a module.   */
/*   For example, "defrule".                                          */
/*                                                                    */
/* allocateFunction: Used to allocate a data structure containing all */
/*   pertinent information related to a specific construct for a      */
/*   given module. For example, the deffacts construct stores a       */
/*   pointer to the first and last deffacts for each each module.     */
/*                                                                    */
/* freeFunction: Used to deallocate a data structure allocated by     */
/*   the allocateFunction. In addition, the freeFunction deletes      */
/*   all constructs of the specified type in the given module.        */
/*                                                                    */
/* bloadModuleReference: Used during a binary load to establish a     */
/*   link between the defmodule data structure and the data structure */
/*   containing all pertinent module information for a specific       */
/*   construct.                                                       */
/*                                                                    */
/* findFunction: Used to determine if a specified construct is in a   */
/*   specific module. The name is the specific construct is passed as */
/*   a string and the function returns a pointer to the specified     */
/*   construct if it exists.                                          */
/*                                                                    */
/* exportable: If true, then the specified construct type can be      */
/*   exported (and hence imported). If false, it can't be exported.   */
/*                                                                    */
/* next: A pointer to the next moduleItem data structure.             */
/**********************************************************************/
#if STUBBING_INACTIVE
typedef void *AllocateModuleFunction(const Environment::Ptr&);
typedef void FreeModuleFunction(const Environment::Ptr&, void *);
typedef ConstructHeader *FindConstructFunction(const Environment::Ptr&, const char *);
typedef ConstructHeader *GetNextConstructFunction(const Environment::Ptr&, ConstructHeader *);
typedef bool IsConstructDeletableFunction(ConstructHeader *);
typedef bool DeleteConstructFunction(ConstructHeader *, const Environment::Ptr&);
typedef void FreeConstructFunction(const Environment::Ptr&, ConstructHeader *);
#endif
struct moduleItem {
public:
    using Self = moduleItem;
    using Ptr = std::shared_ptr<Self>;
public:
    const char *name;
    unsigned moduleIndex;
    AllocateModuleFunction* allocateFunction;
    FreeModuleFunction* freeFunction;
    void *(*bloadModuleReference)(const Environment::Ptr&, unsigned long);
    FindConstructFunction *findFunction;
    Ptr next;
};

struct moduleStackItem {
public:
    using Self = moduleStackItem;
    using Ptr = std::shared_ptr<Self>;
public:
    bool changeFlag;
    Defmodule *theModule;
    Ptr next;
};

constexpr auto DEFMODULE_DATA = 4;

struct defmoduleData : public EnvironmentModule {
    moduleItem::Ptr LastModuleItem;
    struct voidCallFunctionItem *AfterModuleChangeFunctions;
    ModuleStackItem::Ptr ModuleStack;
    bool CallModuleChangeFunctions;
    Defmodule::Ptr ListOfDefmodules;
    Defmodule::Ptr CurrentModule;
    Defmodule::Ptr LastDefmodule;
    unsigned NumberOfModuleItems;
    struct moduleItem *ListOfModuleItems;
    long ModuleChangeIndex;
    bool MainModuleRedefinable;
    struct portConstructItem *ListOfPortConstructItems;
    unsigned short NumberOfDefmodules;
    struct voidCallFunctionItem *AfterModuleDefinedFunctions;
#if (BLOAD_AND_BSAVE)
    unsigned long BNumberOfDefmodules;
    unsigned long NumberOfPortItems;
    struct portItem *PortItemArray;
    Defmodule *DefmoduleArray;
#endif
};
RegisterEnvironmentModule(defmoduleData, DEFMODULE_DATA, Defmodule);

void InitializeDefmodules(const Environment::Ptr&);
Defmodule *FindDefmodule(const Environment::Ptr&, const char *);
const char *DefmoduleName(Defmodule *);
const char *DefmodulePPForm(Defmodule *);
Defmodule *GetNextDefmodule(const Environment::Ptr&, Defmodule *);
void RemoveAllDefmodules(const Environment::Ptr&, void *);
int AllocateModuleStorage();
unsigned RegisterModuleItem(const Environment::Ptr&theEnv,
                            const char *theItem,
                            AllocateModuleFunction *allocateFunction,
                            FreeModuleFunction *freeFunction,
                            void *(*bloadModuleReference)(const Environment::Ptr&, unsigned long),
                            FindConstructFunction *findFunction);
void *GetModuleItem(const Environment::Ptr&, Defmodule *, unsigned);
void SetModuleItem(const Environment::Ptr&, Defmodule *, unsigned, void *);
Defmodule *GetCurrentModule(const Environment::Ptr&);
Defmodule *SetCurrentModule(const Environment::Ptr&, Defmodule *);
void GetCurrentModuleCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
void SetCurrentModuleCommand(const Environment::Ptr&theEnv, UDFContext *context, UDFValue *ret);
unsigned GetNumberOfModuleItems(const Environment::Ptr&);
void CreateMainModule(const Environment::Ptr&, void *);
void SetListOfDefmodules(const Environment::Ptr&, Defmodule *);
struct moduleItem *GetListOfModuleItems(const Environment::Ptr&);
struct moduleItem *FindModuleItem(const Environment::Ptr&, const char *);
void SaveCurrentModule(const Environment::Ptr&);
void RestoreCurrentModule(const Environment::Ptr&);
void AddAfterModuleChangeFunction(const Environment::Ptr&, const char *, VoidCallFunction *, int, void *);
void IllegalModuleSpecifierMessage(const Environment::Ptr&);
void AllocateDefmoduleGlobals(const Environment::Ptr&);
unsigned short GetNumberOfDefmodules(const Environment::Ptr&);

#endif
#endif /* _H_moduldef */


