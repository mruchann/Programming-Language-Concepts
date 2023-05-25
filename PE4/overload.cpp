#include "nullary.h"
#include "unary.h"
#include "binary.h"

namespace sym {
	__expr_t& operator-(const __expr_t &op) {
        if (op.is_nullary()) {
            return * new NegOp(op.eval());
        }
        else {
            return * new NegOp(&op);
        }
    }
	__expr_t& exp(const __expr_t &op) {
        if (op.is_nullary()) {
            return * new ExpOp(op.eval());
        }
        else {
            return * new ExpOp(&op);
        }
    }

	__expr_t& operator+(const __expr_t &lhs, const __expr_t &rhs) {
        if (lhs.is_nullary() && rhs.is_nullary()) {
            return * new AddOp(lhs.eval(), rhs.eval());
        }
        else if (lhs.is_nullary() && !rhs.is_nullary()) {
            return * new AddOp(lhs.eval(), &rhs);
        }
        else if (!lhs.is_nullary() && rhs.is_nullary()) {
            return * new AddOp(&lhs, rhs.eval());
        }
        else if (!lhs.is_nullary() && !rhs.is_nullary()) {
            return * new AddOp(&lhs, &rhs);
        }
    }
	__expr_t& operator+(double lhs, const __expr_t &rhs) {
        if (rhs.is_nullary()) {
            return * new AddOp(new Const(lhs), rhs.eval());
        }
        else {
            return * new AddOp(new Const(lhs), &rhs);
        }
    }
	__expr_t& operator+(const __expr_t &lhs, double rhs) {
        if (lhs.is_nullary()) {
            return * new AddOp(lhs.eval(), new Const(rhs));
        }
        else {
            return * new AddOp(&lhs, new Const(rhs));
        }
    }

	__expr_t& operator*(const __expr_t &lhs, const __expr_t &rhs) {
        if (lhs.is_nullary() && rhs.is_nullary()) {
            return * new MulOp(lhs.eval(), rhs.eval());
        }
        else if (lhs.is_nullary() && !rhs.is_nullary()) {
            return * new MulOp(lhs.eval(), &rhs);
        }
        else if (!lhs.is_nullary() && rhs.is_nullary()) {
            return * new MulOp(&lhs, rhs.eval());
        }
        else if (!lhs.is_nullary() && !rhs.is_nullary()) {
            return * new MulOp(&lhs, &rhs);
        }
    }
	__expr_t& operator*(double lhs, const __expr_t &rhs) {
        if (rhs.is_nullary()) {
            return * new MulOp(new Const(lhs), rhs.eval());
        }
        else {
            return * new MulOp(new Const(lhs), &rhs);
        }
    }
	__expr_t& operator*(const __expr_t &lhs, double rhs) {
        if (lhs.is_nullary()) {
            return * new MulOp(lhs.eval(), new Const(rhs));
        }
        else {
            return * new MulOp(&lhs, new Const(rhs));
        }
    }
}

