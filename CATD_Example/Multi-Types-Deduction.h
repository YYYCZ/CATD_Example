/*****************************************************************//**
 * \file   Multi-Types-Deduction.h
 * \brief  If the intersection of multiple types of parameter packages has only one type,
 *         the type is automatically deduction.
 *
 * \author YYYCZ
 * \date   January 2023
 *********************************************************************/

#include "./Type/Type.h"

namespace yyycz {
	using namespace ytype;

	template<typename... Types>
	using Tuple = std::tuple<Types...>;

	template<typename T>
	using TypeIdentity = T;

	/**
	 * \brief Assitant type to do type deduction of Tuple.
	 * \tparam T the type need to be deduction (Only T = NullType activate)
	 * \tparam P Tuple type
	 * \tparam F the type filter to filter types in Tuple
	 * \tparam M the type mapper to map types in Tuple
	 */
	template<typename T, typename P, typename F = NullType,
		template<typename> typename M = TypeIdentity>
	struct _Tuple_type_deduction {
		using choices = TypeArray<T>;
		using type = T;
	};

	template<typename... Ps, typename F, template<typename> typename M>
	struct _Tuple_type_deduction<NullType, Tuple<Ps...>, F, M> {
		static constexpr auto _Try_filter() noexcept {
			using Ret = TypeArray<Ps...>;
			if constexpr (std::invocable<F, Ret>) {
				using NRet = std::invoke_result_t<F, Ret>;
				if constexpr (isTypeArray<NRet>()) {
					return NRet{};
				}
				else {
					return Ret{};
				}
			}
			else {
				return Ret{};
			}
		}

		template<typename... Types>
		static constexpr TypeArray<M<Types>...> _Map_type(TypeArray<Types...>) noexcept { return {}; }

		static constexpr auto _Get_choices() noexcept {
			return create_type_set(_Map_type(_Try_filter()));
		}

		static constexpr auto _Get_type() noexcept {
			static constexpr auto ta = _Get_choices();
			if constexpr (ta.size() == 1) {
				return ta.front();
			}
			else {
				return NullType{};
			}
		}

		using choices = decltype(_Get_choices());
		using type = decltype(_Get_type());
	};

	/**
	 * \brief Assitant type to deduct unique type in multi TypeArray.
	 * \tparam P TypeArray
	 * \tparam Q TypeArray
	 */
	template<typename P, typename... Q>
	struct _Multi_type_array_unique_deduction {
		static constexpr auto _Get_rest_intersection() noexcept {
			if constexpr (sizeof...(Q) == 0) {
				return NullType{};
			}
			else {
				return _Multi_type_array_unique_deduction<Q...>()._Get_this_intersection();
			}
		}

		static constexpr auto _Get_this_intersection() noexcept {
			if constexpr (isTypeArray<P>()) {
				using T = decltype(_Get_rest_intersection());
				if constexpr (isNullType<T>) {
					return P{};
				}
				else {
					return P{} &T{};
				}
			}
			else {
				return NullType{};
			}
		}

		static constexpr auto _Get_unique_type() noexcept {
			using T = decltype(_Get_this_intersection());
			if constexpr (isNullType<T>) {
				return NullType{};
			}
			else if constexpr (T().size() == 1) {
				return T().front();
			}
			else {
				return NullType{};
			}
		}

		using type = decltype(_Get_unique_type());
	};

	/**
	 * \brief Assitant Function to Multi-Tuples Function's Type Deduction.
	 * \tparam T the type need to be deduction (Only T = NullType activate)
	 * \tparam P Tuple type
	 * \tparam PF Tuple type filter of P
	 * \tparam QF Tuple type filter of Q
	 * \tparam PM Tuple type mapper of P
	 * \tparam QM Tuple type mapper of Q
	 * \tparam Q Tuples type
	 */
	template<typename T, typename P, typename PF, typename QF,
		template<typename> typename PM, template<typename> typename QM, typename... Q>
	static constexpr auto _Multi_tuple_type_deduction() noexcept {
		if constexpr (isNullType<T>) {
			using Ret = typename _Multi_type_array_unique_deduction<
				typename _Tuple_type_deduction<NullType, P, PF, PM>::choices,
				typename _Tuple_type_deduction<NullType, Q, QF, QM>::choices...>::type;
			return Wrapper<Ret>{};
		}
		else {
			return Wrapper<T>{};
		}
	}

	/**
	 * \brief Multi-Tuples Function's Type Deduction.
	 * \tparam T the type need to be deduction (Only T = NullType activate)
	 * \tparam P Tuple type
	 * \tparam PF Tuple type filter of P
	 * \tparam QF Tuple type filter of Q
	 * \tparam PM Tuple type mapper of P
	 * \tparam QM Tuple type mapper of Q
	 * \tparam Q Tuples type
	 */
	template<typename T, typename P, typename PF, typename QF,
		template<typename> typename PM, template<typename> typename QM, typename... Q>
	using MTFTD = getWrapType<decltype(_Multi_tuple_type_deduction<T, P, PF, QF, PM, QM, Q...>())>;
}
