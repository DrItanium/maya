/*******************************************************/
/*      "C" Language Integrated Production System      */
/*                                                     */
/*             CLIPS Version 6.40  11/01/16            */
/*                                                     */
/*                ENTITIES HEADER FILE                 */
/*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.40: Created to store key data structures.          */
/*                                                           */
/*************************************************************/

#ifndef _H_entities
#define _H_entities

#include <memory>
#include <variant>
#include <any>
#include <vector>
#include <functional>
#include <list>
#include <iostream>
#include <experimental/memory>

#include "Constants.h"
#include "Hashable.h"
#include "HoldsEnvironmentCallback.h"
#include "IORouterAware.h"
#include "Evaluable.h"
#include "Callable.h"
#include "ExternalAddress.h"
#include "TypeHeader.h"
#include "Atom.h"
#include "Entity.h"

namespace maya {


    /**************/
/* multifield */
/**************/
    struct Multifield : public HoldsEnvironmentCallback, public TypeHeader, public Evaluable {
    public:
        using Self = Multifield;
        using Ptr = std::shared_ptr<Self>;
    public:
        Multifield(Environment& parent) : HoldsEnvironmentCallback(parent), TypeHeader(MULTIFIELD_TYPE) {}
        ~Multifield() override = default;
        bool evaluate(std::shared_ptr<UDFValue> retVal) override;
    public:
        auto length() const noexcept { return contents.size(); }
        std::vector<std::shared_ptr<struct CLIPSValue>> contents;
    };


#if 0
    /***********************/
/* PatternEntityRecord */
/***********************/
    struct PatternEntityRecord : public EntityRecord {
        PatternEntityRecordDecrementBasisCountFunction decrementBasisCount;
        PatternEntityRecordIncrementBasisCountFunction incrementBasisCount;
        PatternEntityRecordMatchFunction matchFunction;
        PatternEntityRecordSynchronizedFunction synchronized;
        PatternEntityRecordIsDeletedFunction isDeleted;
    };
/*****************/
/* PatternEntity */
/*****************/
    struct PatternEntity : public TypeHeader, public ReferenceCountable {
    public:
        PatternEntity(unsigned short type) : TypeHeader(type) {}
        ~PatternEntity() override = default;
    public:
        std::shared_ptr<struct PatternEntityRecord> theInfo;
        std::any dependents;
    private:
        unsigned _busyCount = 0;
        unsigned long long _timeTag = 0;
    public:
        void retain() override;
        void release() override;
        bool canRelease() const noexcept override;
        constexpr auto getBusyCount() const noexcept { return _busyCount; }
        constexpr auto getTimeTag() const noexcept { return _timeTag; }
        void setTimetag(unsigned long long value) noexcept { _timeTag = value; }
    };
#endif
    class PatternEntity : public Entity {
    public:
        using Self = PatternEntity;
        using Ptr = std::shared_ptr<Self>;
    public:
        using Entity::Entity;
        ~PatternEntity() override = default;
        virtual void onMatch();
        virtual bool synchronized();
        [[nodiscard]] constexpr auto getTimeTag() const noexcept { return _timeTag; }
        void setTimeTag(unsigned long long value) noexcept { _timeTag = value; }
        auto getDependents() const noexcept { return _dependents; }
        void setDependents(std::any value) { _dependents = value; }
    private:
        unsigned long long _timeTag = 0;
        std::any _dependents;
    };
} // end namespace maya

#endif /* _H_entities */


