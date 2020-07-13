//
// Created by jwscoggins on 6/21/20.
//

#ifndef MAYA_EXTERNALADDRESSTRACKER_H
#define MAYA_EXTERNALADDRESSTRACKER_H
#include <map>
#include "Problem.h"
#include "clips.h"
namespace maya {
        /**
         * @brief Track the type code generated from registering a type
         */
        template<typename T>
        class ExternalAddressTracker final {
            public:
            ExternalAddressTracker() = delete;
            ~ExternalAddressTracker() = delete;
            ExternalAddressTracker(ExternalAddressTracker&&) = delete;
            ExternalAddressTracker(const ExternalAddressTracker&) = delete;
            ExternalAddressTracker& operator=(const ExternalAddressTracker&) = delete;
            ExternalAddressTracker& operator=(ExternalAddressTracker&&) = delete;
            static int getExternalAddressId(const Environment::Ptr& env) {
                return getExternalAddressId_Internal(env.get());
            }
            static void registerExternalAddressId(const Environment::Ptr& env, int result) {
                registerExternalAddressId_Internal(env.get(), result);
            }
            private:
            static int getExternalAddressId_Internal(void* env) {
                if (auto found = _cache.find(env); found != _cache.end()) {
                    return found->second;
                } else {
                    throw Problem("Attempted to get the external address index of something not registered from using an unregistered environment");
                }
            }
            static void registerExternalAddressId_Internal(void* env, int result) {
                _cache.emplace(env, result);
            }
            static std::map<void*, int> _cache;
        };
        template<typename T>
        std::map<void*, int> ExternalAddressTracker<T>::_cache;

}
#endif //MAYA_EXTERNALADDRESSTRACKER_H
