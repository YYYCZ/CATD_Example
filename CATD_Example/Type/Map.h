/*****************************************************************//**
 * \file   Map.h
 * \brief  Some map functions of Types.
 *
 * \author YYYCZ
 * \date   December 2022
 *********************************************************************/

#ifndef __YTYPE_MAP_H__
#define __YTYPE_MAP_H__

#include "Set.h"

namespace ytype {
	/**
	 * \brief Add Val to integer_sequence<T>{Vals...} if {Tests...}.
	 * \tparam T
	 * \tparam Val
	 * \tparam Tests
	 * \tparam Vals
	 * \param seq vals deduction
	 * \return integer_sequence<T>{(Val + Vals if Tests else Vals)...}
	 */
	template<typename T, T Val, bool... Tests, T... Vals>
	static constexpr std::enable_if_t<sizeof...(Tests) == sizeof...(Vals),
		std::integer_sequence<T, (Tests ? Val + Vals : Vals)...>>
		_Add_val_to_vals_if(std::integer_sequence<T, Vals...> seq = {}) noexcept { return {}; }

	/**
	 * \brief Assitant type to map types to its nth type.
	 * \tparam T the type of integer_sequence
	 * \tparam Types
	 */
	template<typename T, typename... Types>
	struct _Map_to_nth_type {
		using type = std::integer_sequence<T>;
	};

	template<typename T, typename P, typename... Types>
	struct _Map_to_nth_type<T, P, Types...> {
		using indexes_array = typename _Map_to_nth_type<T, Types...>::type;
		using type = decltype(_Choice_to_push_front_is<true, T, T(0)>(
			_Add_val_to_vals_if<T, T(1), std::is_same_v<P, Types>...>(indexes_array{})));
	};

	/**
	 * \brief Map types to its nth type.
	 * \tparam T the type of integer_sequence
	 * \tparam Types
	 * \return integer_sequence<T>
	 */
	template<typename T, typename... Types>
	using _MapToNthType = typename _Map_to_nth_type<T, Types...>::type;

	/**
	 * \brief Assitant type to find nth F in P.
	 * \tparam P TypeArray pattern
	 * \tparam F TypeArray be sought
	 * \tparam Ns corresponding nth type
	 */
	template<typename P, typename F, size_t... Ns>
	struct _Find_nth_multi_times {
		using type = std::integer_sequence<size_t>;
	};

	template<typename... Types, typename T, typename... Ts, size_t N, size_t... Ns>
	struct _Find_nth_multi_times<TypeArray<Types...>, TypeArray<T, Ts...>, N, Ns...> {
		using pattern_array = TypeArray<Types...>;
		using find_array = TypeArray<Ts...>;
		static constexpr size_t index = pattern_array().find_nth<T, N>();
		using indexes_array = typename _Find_nth_multi_times<pattern_array, find_array, Ns...>::type;
		using type = decltype(_Choice_to_push_front_is<true, size_t, index>(indexes_array{}));
	};

	/**
	 * \brief Find nth F in P.
	 * \tparam P TypeArray pattern
	 * \tparam F TypeArray be sought
	 * \tparam Ns corresponding nth type
	 * \return index_sequence
	 */
	template<typename P, typename F, size_t... Ns>
	using _FindNthMultiTimes = typename _Find_nth_multi_times<P, F, Ns...>::type;

	/**
	 * \brief Assitant function to distribute types a to types b.
	 * \tparam Ns
	 * \tparam Ts
	 * \tparam Ps
	 * \param _ Ns deduction
	 * \param a Ts deduction
	 * \param b Ps deduction
	 * \return the indexes
	 */
	template<size_t... Ns, typename... Ts, typename... Ps>
	static constexpr auto _Distribute_types_by(std::integer_sequence<size_t, Ns...> _ = {},
		TypeArray<Ts...> a = {}, TypeArray<Ps...> b = {}) noexcept {
		return _FindNthMultiTimes<TypeArray<Ps...>, TypeArray<Ts...>, Ns...>{};
	}

	/**
	 * \brief Distribute every type in TypeArray<Ts...> to proper indexes of TypeArray<Ps...>.
	 *        If can not distribute(TypeArray<Ts...> is not subarray of TypeArray<Ps...>), return NullType.
	 * \tparam Ts
	 * \tparam Ps
	 * \param a Ts deduction
	 * \param b Ps deduction
	 * \return the indexes or NullType
	 */
	template<typename ...Ts, typename... Ps>
	static constexpr auto _Distribute_types_in(TypeArray<Ts...> a = {}, TypeArray<Ps...> b = {}) noexcept {
		if constexpr (b.can_distribute(a)) {
			return _Distribute_types_by(a.nth(), a, b);
		}
		else {
			return NullType{};
		}
	}
}

namespace ytype {
	/**
	 * \brief Map integer_sequence<T, Ns...> to TypeArray<integer_sequence<T, Ns>...>.
	 * \tparam T
	 * \tparam Ns
	 * \param _ T and Ns deduction
	 * \return TypeArray<integer_sequence<T, Ns>...>
	 */
	template<typename T, size_t... Ns>
	static constexpr TypeArray<std::integer_sequence<T, Ns>...> map_value_to_type(std::integer_sequence<T, Ns...> = {}) noexcept {
		return {};
	}

	/**
	 * \brief Map (T, Ns...) to TypeArray<integer_sequence<T, Ns>...>.
	 * \tparam T
	 * \tparam Ns
	 * \return TypeArray<integer_sequence<T, Ns>...>
	 */
	template<typename T, size_t... Ns>
	using MapValueToType = TypeArray<std::integer_sequence<T, Ns>...>;

	/**
	 * \brief Map to nth type of types.
	 * \return the indexes
	 */
	template<typename... Types>
	inline constexpr auto TypeArray<Types...>::nth() noexcept {
		return _MapToNthType<size_t, Types...>{};
	}

	/**
	 * \brief Distribute every type in TypeArray<Ts...> to proper indexes of TypeArray<Types...>.
	 *        If can not distribute(TypeArray<Ts...> is not subarray of TypeArray<Types...>), return NullType.
	 * \tparam Ts
	 * \param _ Ts deduction
	 * \return the indexes or NullType
	 */
	template<typename... Types>
	template<typename... Ts>
	static constexpr auto TypeArray<Types...>::distribute(TypeArray<Ts...> _) noexcept {
		return _Distribute_types_in(_, TypeArray<Types...>{});
	}

	/**
	 * \brief Check if TypeArray<Types...> can distribute TypeArray<Ts...>.
	 * \tparam Ts
	 * \param _ Ts deduction
	 * \return if can distribute
	 */
	template<typename... Types>
	template<typename... Ts>
	static constexpr bool TypeArray<Types...>::can_distribute(TypeArray<Ts...> _) noexcept {
		return is_sub_type_array(_, TypeArray<Types...>{});
	}
}

#endif
