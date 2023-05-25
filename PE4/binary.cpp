#include "binary.h"
#include "nullary.h"
#include <math.h>

namespace sym
{
	bool AddOp::is_add() const {
        return true;
    }

	__expr_t* AddOp::eval(const var_map_t& vars) const {
        __expr_t *tl = lhs_->eval(vars);
        __expr_t *tr = rhs_->eval(vars);
        const Const* l = dynamic_cast<const Const*>(tl);
        const Const* r = dynamic_cast<const Const*>(tr);

        if (l && r) {
            double res = l->get_value() + r->get_value();
            delete tl; delete tr;
            return new Const(res);
        }
        else if (l && l->get_value() == 0) {
            delete tl;
            return tr;
        }
        else if (r && r->get_value() == 0) {
            delete tr;
            return tl;
        }
        else {
            return new AddOp(tl, tr);
        }
    }

    __expr_t* AddOp::diff(const std::string& v) const {
        __expr_t *tl = lhs_->diff(v);
        __expr_t *tr = rhs_->diff(v);

        __expr_t *res = new AddOp(tl->eval(), tr->eval());
        delete tl; delete tr;

        __expr_t *final_res = res->eval();
        delete res;

        return final_res;
    }

	std::ostream& AddOp::operator<< (std::ostream &out) const {
        const Const *l = dynamic_cast<const Const*>(lhs_);
        const Const *r = dynamic_cast<const Const*>(rhs_);
        if (l && l->get_value() == 0) {
            return out << *rhs_;
        }
        else if (r && r->get_value() == 0) {
            return out << *lhs_;
        }
        else {
            if (lhs_->is_nullary() && rhs_->is_nullary()) {
                return out << *lhs_ << " + " << *rhs_;
            }
            else if (!lhs_->is_nullary() && rhs_->is_nullary()) {
                return out << "(" << *lhs_ << ")" << " + " << *rhs_;
            }
            else if (lhs_->is_nullary() && !rhs_->is_nullary()) {
                return out << *lhs_ << " + " << "(" << *rhs_ << ")";
            }
            else if (!lhs_->is_nullary() && !rhs_->is_nullary()) {
                return out << "(" << *lhs_ << ")" << " + " << "(" << *rhs_ << ")";
            }
        }
    }

	bool AddOp::operator==(const __expr_t& other_) const {
        const AddOp *p = dynamic_cast<const AddOp *>(&other_);
        if (!p) {
            return false;
        }
        return ((*lhs_ == *p->lhs_) && (*rhs_ == *p->rhs_))
               ||
               ((*lhs_ == *p->rhs_) && (*rhs_ == *p->lhs_));
    }
}

namespace sym
{
	bool MulOp::is_mul() const {
        return true;
    }

	__expr_t* MulOp::eval(const var_map_t& vars) const {
        __expr_t *tl = lhs_->eval(vars);
        __expr_t *tr = rhs_->eval(vars);

        const Const* l = dynamic_cast<const Const*>(tl);
        const Const* r = dynamic_cast<const Const*>(tr);

        if (l && r) {
            double res = l->get_value() * r->get_value();
            delete tl; delete tr;
            return new Const(res);
        }
        else if ((l && l->get_value() == 0) || (r && r->get_value() == 0)) {
            delete tl; delete tr;
            return new Const(0);
        }
        else if (l && l->get_value() == 1) {
            delete tl;
            return tr;
        }
        else if (r && r->get_value() == 1) {
            delete tr;
            return tl;
        }
        else {
            return new MulOp(tl, tr);
        }
    }

    __expr_t* MulOp::diff(const std::string& v) const {
        __expr_t *tl = lhs_->diff(v);
        __expr_t *tr = rhs_->diff(v);

        __expr_t *l = new MulOp(tl->eval(), rhs_->eval());
        __expr_t *r = new MulOp(lhs_->eval(), tr->eval());
        delete tl; delete tr;

        __expr_t *res = new AddOp(l->eval(), r->eval());
        delete l; delete r;

        __expr_t *final_res = res->eval();
        delete res;

        return final_res;
    }

	std::ostream& MulOp::operator<< (std::ostream &out) const {
        const Const *l = dynamic_cast<const Const*>(lhs_);
        const Const *r = dynamic_cast<const Const*>(rhs_);

        if ( (l && (l->get_value() == 0)) || (r && (r->get_value() == 0)) ) {
            return out << Const(0);
        }
        else if (l && l->get_value() == 1) {
            return out << *rhs_;
        }
        else if (r && r->get_value() == 1) {
            return out << *lhs_;
        }
        else {
            if (lhs_->is_nullary() && rhs_->is_nullary()) {
                return out << *lhs_ << " * " << *rhs_;
            }
            else if (!lhs_->is_nullary() && rhs_->is_nullary()) {
                return out << "(" << *lhs_ << ")" << " * " << *rhs_;
            }
            else if (lhs_->is_nullary() && !rhs_->is_nullary()) {
                return out << *lhs_ << " * " << "(" << *rhs_ << ")";
            }
            else if (!lhs_->is_nullary() && !rhs_->is_nullary()) {
                return out << "(" << *lhs_ << ")" << " * " << "(" << *rhs_ << ")";
            }
        }
    }

	bool MulOp::operator==(const __expr_t& other_) const {
        const MulOp *p = dynamic_cast<const MulOp *>(&other_);
        if (!p) {
            return false;
        }
        return ((*lhs_ == *p->lhs_) && (*rhs_ == *p->rhs_))
               ||
               ((*lhs_ == *p->rhs_) && (*rhs_ == *p->lhs_));
    }
}
