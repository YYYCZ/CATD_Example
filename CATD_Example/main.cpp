/*****************************************************************//**
 * \file   main.cpp
 * \brief  自定义自动类型推导的案例。
 *
 * \author YYYCZ
 * \date   January 2023
 *********************************************************************/

#include <iostream>
using namespace std;

// 自动推导的实现部分
#include "Multi-Types-Deduction.h"

namespace yyycz {
	/**
	 * \brief 检查多个元组中的某个类型的值是否全都相等.
	 * \tparam T 要被检查的类型，如果不填，可以根据情况自动推导（详见DT部分）。
	 * \param tpl 元组
	 * \param tpls 元组
	 * \return 是否相等
	 */
	template<
		typename T = NullType,
		typename _Tuple,
		typename... _Tuples,
		typename DT = MTFTD<T, std::decay_t<_Tuple>, NullType, NullType, TypeIdentity, TypeIdentity, std::decay_t<_Tuples>...>
	>
	static constexpr std::enable_if_t<!isNullType<DT>, bool>
		is_same_in_tuples(_Tuple&& tpl, _Tuples&&... tpls) noexcept {
		return ((std::get<DT>(tpl) == std::get<DT>(tpls)) && ...);
	}
}

int main() {
	using namespace yyycz;

	Tuple<int, float, double> tpl1(1, 2, 3);
	Tuple<double, char, int> tpl2(2, 1, 1);
	Tuple<float, bool> tpl3(3, true);
	Tuple<double, char> tpl4(1, 0);

	// 1. 多个类型匹配，推导失败，只能手动指定类型
	is_same_in_tuples(tpl1, tpl2);
	is_same_in_tuples(tpl2, tpl4);
	is_same_in_tuples<int>(tpl1, tpl2);
	is_same_in_tuples<char>(tpl2, tpl4);

	// 2. 只有一个类型匹配，推导成功
	is_same_in_tuples(tpl1, tpl3);
	is_same_in_tuples(tpl1, tpl4);
	is_same_in_tuples(tpl1, tpl2, tpl4);

	// 3. 没有类型匹配，推导失败
	is_same_in_tuples(tpl1, tpl2, tpl3, tpl4);

	return 0;
}
