#include "nullary.h"
#include "unary.h"
#include "binary.h"
#include <math.h>

namespace sym 
{
	bool NegOp::is_neg() const {
        return true;
    }

	__expr_t* NegOp::eval(const var_map_t& vars) const {
        __expr_t *t = operand->eval(vars);
        const Const *p = dynamic_cast<const Const*>(t);
        if (p) {
            double res = -p->get_value();
            delete t;
            return new Const(res);
        }
        else {
            return new NegOp(t);
        }
    }

	__expr_t* NegOp::diff(const std::string& v) const {
        __expr_t *t = operand->diff(v);
        __expr_t *res = new NegOp(t->eval());
        delete t;
        return res;
    }

	std::ostream& NegOp::operator<< (std::ostream &out) const {
        if (operand->is_nullary()) {
            out << "-" << *operand;
        }
        else {
            out << "-" << "(" << *operand << ")";
        }
    }

	bool NegOp::operator==(const __expr_t& other_) const {
        const NegOp *p = dynamic_cast<const NegOp *>(&other_);
        if (!p) {
            return false;
        }
        return *operand == *p->operand;
    }
}

namespace sym 
{
	bool ExpOp::is_exp() const {
        return true;
    }

	__expr_t* ExpOp::eval(const var_map_t& vars) const {
        const __expr_t *t = operand->eval(vars);
        const Const * p = dynamic_cast<const Const*>(t);
        if (p) {
            double res = std::exp(p->get_value());
            delete t;
            return new Const(res);
        }
        else {
            return new ExpOp(t);
        }
    }

	__expr_t* ExpOp::diff(const std::string& v) const {
        __expr_t *t = operand->diff(v);
        __expr_t *res = new MulOp(t->eval(), new ExpOp(operand->eval()));
        delete t;
        return res;
    }

	std::ostream& ExpOp::operator<< (std::ostream &out) const {
        if (operand->is_nullary()) {
            return out << "e^" << *operand;
        }
        else {
            return out << "e^" << "(" << *operand << ")";
        }
    }

	bool ExpOp::operator==(const __expr_t& other_) const {
        const ExpOp *p = dynamic_cast<const ExpOp *>(&other_);
        if (!p) {
            return false;
        }
        return *operand == *p->operand;
    }
}
