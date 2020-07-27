//
// Created by jwscoggins on 7/26/20.
//

#ifndef MAYA_TRANSFEREVALUABLE_H
#define MAYA_TRANSFEREVALUABLE_H
#include "Evaluable.h"
#include "Value.h"
#include <memory>
namespace maya {
    /**
     * @brief An implementation of Evaluable where the contents of this type are returned
     * @tparam T The type of the class to store within the return value
     */
    template<typename T>
    class TransferEvaluable : public Evaluable, std::enable_shared_from_this<T> {
    public:
        ~TransferEvaluable() override = default;
        bool evaluate(std::shared_ptr<UDFValue> retVal) override {
            retVal->setContents(this->shared_from_this());
            return true;
        }
    };
}
#endif //MAYA_TRANSFEREVALUABLE_H
