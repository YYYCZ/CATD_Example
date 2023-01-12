/*****************************************************************//**
 * \file   Tuple.h
 * \brief  Some utils to help to use std::tuple.
 *
 * \author YYYCZ
 * \date   December 2022
 *********************************************************************/

#ifndef __YTYPE_TUPLE_H__
#define __YTYPE_TUPLE_H__

#include "Map.h"
#include <tuple>

namespace ytype {
	/**
	 * \brief Assitant type to check if T is tuple and convert to TypeArray.
	 * \tparam T
	 */
	template<typename T>
	struct _Type_array_from_tuple : std::false_type {
		using type = TypeArray<T>;
	};

	template<typename... Types>
	struct _Type_array_from_tuple<std::tuple<Types...>> : std::true_type {
		using type = TypeArray<Types...>;
	};

	/**
	 * \brief Convert tuple to TypeArray.
	 * \tparam T
	 * \return TypeArray
	 */
	template<typename T>
	using _Convert_tuple_to_type_array = typename _Type_array_from_tuple<std::decay_t<T>>::type;

	/**
	 * \brief Check if T is std::tuple.
	 * \tparam T
	 * \return if T is std::tuple
	 */
	template<typename T>
	static constexpr bool _Is_std_tuple = _Type_array_from_tuple<std::decay_t<T>>::value;
}

namespace ytype {
	/**
	 * \brief Create TypeArray from std::tuple.
	 * \tparam T tuple type
	 * \return TypeArray
	 */
	template<typename T>
	static constexpr std::enable_if_t<_Is_std_tuple<T>, _Convert_tuple_to_type_array<T>>
		create_type_array_from_tuple() noexcept { return {}; }

	/**
	 * \brief Create TypeArray from std::tuple.
	 * \tparam T tuple type
	 * \param _ T deduction
	 * \return TypeArray
	 */
	template<typename T>
	static constexpr std::enable_if_t<_Is_std_tuple<T>, _Convert_tuple_to_type_array<T>>
		create_type_array_from_tuple(T&&) noexcept { return {}; }

	/**
	 * \brief Get std tuple size of T safely.
	 * \tparam T
	 * \return tuple size
	 */
	template<typename T>
	static constexpr size_t tuple_size_s() noexcept {
		using P = std::decay_t<T>;
		if constexpr (_Is_std_tuple<P>) {
			return std::tuple_size_v<P>;
		}
		else {
			return (size_t)1;
		}
	}

	/**
	 * \brief Get std tuple size of T safely.
	 * \tparam T
	 * \return tuple size
	 */
	template<typename T>
	static constexpr size_t tuple_size_s_v = tuple_size_s<T>();

	/**
	 * \brief Get Nth std tuple element of T safely.
	 * \tparam N
	 * \tparam T
	 * \return element
	 */
	template<size_t N, typename T>
	static constexpr auto tuple_element_s() noexcept {
		using P = std::decay_t<T>;
		if constexpr (_Is_std_tuple<P>) {
			return _Move_cref_to<T, std::tuple_element_t<N, P>>();
		}
		else {
			return Wrapper<T>{};
		}
	}

	/**
	 * \brief Get Nth std tuple element of T safely.
	 * \tparam N
	 * \tparam T
	 * \return element type
	 */
	template<size_t N, typename T>
	using tuple_element_s_t = getWrapType<decltype(tuple_element_s<N, T>())>;

	/**
	 * \brief Pick Ns values in tuple.
	 * \tparam Ns
	 * \tparam Types types of tuple
	 * \param tpl tuple
	 * \param _ Ns deduction
	 * \return values in tuple
	 */
	template<size_t... Ns, typename... Types>
	[[nodiscard]] constexpr std::enable_if_t<sizeof...(Ns) && ((Ns < sizeof...(Types)) && ...),
		std::tuple<decltype(std::get<Ns>(std::declval<std::tuple<Types...>&>()))...>>
		pick(std::tuple<Types...>& tpl, std::integer_sequence<size_t, Ns...> = {}) noexcept {
		return std::tuple<decltype(std::get<Ns>(tpl))...>(std::get<Ns>(tpl)...);
	}

	/**
	 * \brief Pick Ns values in tuple.
	 * \tparam Ns
	 * \tparam Types types of tuple
	 * \param tpl tuple
	 * \param _ Ns deduction
	 * \return values in tuple
	 */
	template<size_t... Ns, typename... Types>
	[[nodiscard]] constexpr std::enable_if_t<sizeof...(Ns) && ((Ns < sizeof...(Types)) && ...),
		std::tuple<decltype(std::get<Ns>(std::declval<const std::tuple<Types...>&>()))...>>
		pick(const std::tuple<Types...>& tpl, std::integer_sequence<size_t, Ns...> = {}) noexcept {
		return std::tuple<decltype(std::get<Ns>(tpl))...>(std::get<Ns>(tpl)...);
	}

	/**
	 * \brief Pick Ns values in tuple.
	 * \tparam Ns
	 * \tparam Types types of tuple
	 * \param tpl tuple
	 * \param _ Ns deduction
	 * \return values in tuple
	 */
	template<size_t... Ns, typename... Types>
	[[nodiscard]] constexpr std::enable_if_t<sizeof...(Ns) && ((Ns < sizeof...(Types)) && ...),
		std::tuple<decltype(std::get<Ns>(std::declval<std::tuple<Types...>&&>()))...>>
		pick(std::tuple<Types...>&& tpl, std::integer_sequence<size_t, Ns...> = {}) noexcept {
		return std::tuple<decltype(std::get<Ns>(std::move(tpl)))...>(std::get<Ns>(std::move(tpl))...);
	}

	/**
	 * \brief Pick Ns values in tuple.
	 * \tparam Ns
	 * \tparam Types types of tuple
	 * \param tpl tuple
	 * \param _ Ns deduction
	 * \return values in tuple
	 */
	template<size_t... Ns, typename... Types>
	[[nodiscard]] constexpr std::enable_if_t<sizeof...(Ns) && ((Ns < sizeof...(Types)) && ...),
		std::tuple<decltype(std::get<Ns>(std::declval<const std::tuple<Types...>&&>()))...>>
		pick(const std::tuple<Types...>&& tpl, std::integer_sequence<size_t, Ns...> = {}) noexcept {
		return std::tuple<decltype(std::get<Ns>(std::move(tpl)))...>(std::get<Ns>(std::move(tpl))...);
	}

	/**
	 * \brief Pick Ts values in tuple. Same Types will pick in order.
	 * \tparam Ts
	 * \tparam Types types of tuple
	 * \param tpl tuple
	 * \param _ Ts deduction
	 * \return values in tuple
	 */
	template<typename... Ts, typename... Types>
	[[nodiscard]] constexpr std::enable_if_t<TypeArray<Types...>().can_distribute(TypeArray<Ts...>()),
		std::tuple<Ts&...>> pick(std::tuple<Types...>& tpl, TypeArray<Ts...> = {}) noexcept {
		return pick(tpl, TypeArray<Types...>().distribute(TypeArray<Ts...>()));
	}

	/**
	 * \brief Pick Ts values in tuple. Same Types will pick in order.
	 * \tparam Ts
	 * \tparam Types types of tuple
	 * \param tpl tuple
	 * \param _ Ts deduction
	 * \return values in tuple
	 */
	template<typename... Ts, typename... Types>
	[[nodiscard]] constexpr std::enable_if_t<TypeArray<Types...>().can_distribute(TypeArray<Ts...>()),
		std::tuple<const Ts&...>> pick(const std::tuple<Types...>& tpl, TypeArray<Ts...> = {}) noexcept {
		return pick(tpl, TypeArray<Types...>().distribute(TypeArray<Ts...>()));
	}

	/**
	 * \brief Pick Ts values in tuple. Same Types will pick in order.
	 * \tparam Ts
	 * \tparam Types types of tuple
	 * \param tpl tuple
	 * \param _ Ts deduction
	 * \return values in tuple
	 */
	template<typename... Ts, typename... Types>
	[[nodiscard]] constexpr std::enable_if_t<TypeArray<Types...>().can_distribute(TypeArray<Ts...>()),
		std::tuple<Ts&&...>> pick(std::tuple<Types...>&& tpl, TypeArray<Ts...> = {}) noexcept {
		return pick(std::move(tpl), TypeArray<Types...>().distribute(TypeArray<Ts...>()));
	}

	/**
	 * \brief Pick Ts values in tuple. Same Types will pick in order.
	 * \tparam Ts
	 * \tparam Types types of tuple
	 * \param tpl tuple
	 * \param _ Ts deduction
	 * \return values in tuple
	 */
	template<typename... Ts, typename... Types>
	[[nodiscard]] constexpr std::enable_if_t<TypeArray<Types...>().can_distribute(TypeArray<Ts...>()),
		std::tuple<const Ts&&...>> pick(const std::tuple<Types...>&& tpl, TypeArray<Ts...> = {}) noexcept {
		return pick(std::move(tpl), TypeArray<Types...>().distribute(TypeArray<Ts...>()));
	}

	/**
	 * \brief Pick Ns values with transform in tuple.
	 * \tparam Ns
	 * \tparam Types types of tuple
	 * \tparam Transform Can be a function or functions in tuple(must be same size with data tuple)
	 * \param tpl tuple
	 * \param _ Ns deduction
	 * \return values in tuple
	 */
	template<size_t... Ns, typename... Types, typename Transform>
	[[nodiscard]] constexpr std::enable_if_t<sizeof...(Ns) && ((Ns < sizeof...(Types)) && ...) && ((
		_Is_std_tuple<Transform>&& tuple_size_s_v<Transform> == sizeof...(Types)
		&& (std::is_invocable_v<tuple_element_s_t<Ns, Transform>, tuple_element_s_t<Ns, std::tuple<Types...>>&> && ...))
		|| (!_Is_std_tuple<Transform> && (std::is_invocable_v<Transform, tuple_element_s_t<Ns, std::tuple<Types...>>&> && ...))),
		std::tuple<decltype(std::declval<tuple_element_s_t<Ns, Transform>>()(std::get<Ns>(std::declval<std::tuple<Types...>&>())))...>>
		pick(std::tuple<Types...>& tpl, Transform&& transform, std::integer_sequence<size_t, Ns...> = {}) noexcept {
		if constexpr (_Is_std_tuple<Transform>) {
			return std::tuple<decltype(std::get<Ns>(std::forward<Transform>(transform))(std::get<Ns>(tpl)))...>
				(std::get<Ns>(std::forward<Transform>(transform))(std::get<Ns>(tpl))...);
		}
		else {
			return std::tuple<decltype(transform(std::get<Ns>(tpl)))...>(transform(std::get<Ns>(tpl))...);
		}
	}

	/**
	 * \brief Pick Ns values with transform in tuple.
	 * \tparam Ns
	 * \tparam Types types of tuple
	 * \tparam Transform Can be a function or functions in tuple(must be same size with data tuple)
	 * \param tpl tuple
	 * \param _ Ns deduction
	 * \return values in tuple
	 */
	template<size_t... Ns, typename... Types, typename Transform>
	[[nodiscard]] constexpr std::enable_if_t<sizeof...(Ns) && ((Ns < sizeof...(Types)) && ...) && ((
		_Is_std_tuple<Transform>&& tuple_size_s_v<Transform> == sizeof...(Types)
		&& (std::is_invocable_v<tuple_element_s_t<Ns, Transform>, const tuple_element_s_t<Ns, std::tuple<Types...>>&> && ...))
		|| (!_Is_std_tuple<Transform> && (std::is_invocable_v<Transform, const tuple_element_s_t<Ns, std::tuple<Types...>>&> && ...))),
		std::tuple<decltype(std::declval<tuple_element_s_t<Ns, Transform>>()(std::get<Ns>(std::declval<const std::tuple<Types...>&>())))...>>
		pick(const std::tuple<Types...>& tpl, Transform&& transform, std::integer_sequence<size_t, Ns...> = {}) noexcept {
		if constexpr (_Is_std_tuple<Transform>) {
			return std::tuple<decltype(std::get<Ns>(std::forward<Transform>(transform))(std::get<Ns>(tpl)))...>
				(std::get<Ns>(std::forward<Transform>(transform))(std::get<Ns>(tpl))...);
		}
		else {
			return std::tuple<decltype(transform(std::get<Ns>(tpl)))...>(transform(std::get<Ns>(tpl))...);
		}
	}

	/**
	 * \brief Pick Ns values with transform in tuple.
	 * \tparam Ns
	 * \tparam Types types of tuple
	 * \tparam Transform Can be a function or functions in tuple(must be same size with data tuple)
	 * \param tpl tuple
	 * \param _ Ns deduction
	 * \return values in tuple
	 */
	template<size_t... Ns, typename... Types, typename Transform>
	[[nodiscard]] constexpr std::enable_if_t<sizeof...(Ns) && ((Ns < sizeof...(Types)) && ...) && ((
		_Is_std_tuple<Transform>&& tuple_size_s_v<Transform> == sizeof...(Types)
		&& (std::is_invocable_v<tuple_element_s_t<Ns, Transform>, tuple_element_s_t<Ns, std::tuple<Types...>>&&> && ...))
		|| (!_Is_std_tuple<Transform> && (std::is_invocable_v<Transform, tuple_element_s_t<Ns, std::tuple<Types...>>&&> && ...))),
		std::tuple<decltype(std::declval<tuple_element_s_t<Ns, Transform>>()(std::get<Ns>(std::declval<std::tuple<Types...>&&>())))...>>
		pick(std::tuple<Types...>&& tpl, Transform&& transform, std::integer_sequence<size_t, Ns...> = {}) noexcept {
		if constexpr (_Is_std_tuple<Transform>) {
			return std::tuple<decltype(std::get<Ns>(std::forward<Transform>(transform))(std::get<Ns>(tpl)))...>
				(std::get<Ns>(std::forward<Transform>(transform))(std::get<Ns>(tpl))...);
		}
		else {
			return std::tuple<decltype(transform(std::get<Ns>(tpl)))...>(transform(std::get<Ns>(tpl))...);
		}
	}

	/**
	 * \brief Pick Ns values with transform in tuple.
	 * \tparam Ns
	 * \tparam Types types of tuple
	 * \tparam Transform Can be a function or functions in tuple(must be same size with data tuple)
	 * \param tpl tuple
	 * \param _ Ns deduction
	 * \return values in tuple
	 */
	template<size_t... Ns, typename... Types, typename Transform>
	[[nodiscard]] constexpr std::enable_if_t<sizeof...(Ns) && ((Ns < sizeof...(Types)) && ...) && ((
		_Is_std_tuple<Transform>&& tuple_size_s_v<Transform> == sizeof...(Types)
		&& (std::is_invocable_v<tuple_element_s_t<Ns, Transform>, const tuple_element_s_t<Ns, std::tuple<Types...>>&&> && ...))
		|| (!_Is_std_tuple<Transform> && (std::is_invocable_v<Transform, const tuple_element_s_t<Ns, std::tuple<Types...>>&&> && ...))),
		std::tuple<decltype(std::declval<tuple_element_s_t<Ns, Transform>>()(std::get<Ns>(std::declval<const std::tuple<Types...>&&>())))...>>
		pick(const std::tuple<Types...>&& tpl, Transform&& transform, std::integer_sequence<size_t, Ns...> = {}) noexcept {
		if constexpr (_Is_std_tuple<Transform>) {
			return std::tuple<decltype(std::get<Ns>(std::forward<Transform>(transform))(std::get<Ns>(tpl)))...>
				(std::get<Ns>(std::forward<Transform>(transform))(std::get<Ns>(tpl))...);
		}
		else {
			return std::tuple<decltype(transform(std::get<Ns>(tpl)))...>(transform(std::get<Ns>(tpl))...);
		}
	}

	/**
	 * \brief Pick Ts values with transform in tuple. Same Types will pick in order.
	 * \tparam Ts
	 * \tparam Types types of tuple
	 * \tparam Transform Can be a function or functions in tuple(must be same size with data tuple)
	 * \param tpl tuple
	 * \param _ Ts deduction
	 * \return values in tuple
	 */
	template<typename... Ts, typename... Types, typename Transform,
		typename = std::enable_if_t<TypeArray<Types...>().can_distribute(TypeArray<Ts...>())>>
		[[nodiscard]] constexpr auto pick(std::tuple<Types...>& tpl, Transform&& transform, TypeArray<Ts...> = {}) noexcept {
		return pick(tpl, std::forward<Transform>(transform), TypeArray<Types...>().distribute(TypeArray<Ts...>()));
	}

	/**
	 * \brief Pick Ts values with transform in tuple. Same Types will pick in order.
	 * \tparam Ts
	 * \tparam Types types of tuple
	 * \tparam Transform Can be a function or functions in tuple(must be same size with data tuple)
	 * \param tpl tuple
	 * \param _ Ts deduction
	 * \return values in tuple
	 */
	template<typename... Ts, typename... Types, typename Transform,
		typename = std::enable_if_t<TypeArray<Types...>().can_distribute(TypeArray<Ts...>())>>
		[[nodiscard]] constexpr auto pick(const std::tuple<Types...>& tpl, Transform&& transform, TypeArray<Ts...> = {}) noexcept {
		return pick(tpl, std::forward<Transform>(transform), TypeArray<Types...>().distribute(TypeArray<Ts...>()));
	}

	/**
	 * \brief Pick Ts values with transform in tuple. Same Types will pick in order.
	 * \tparam Ts
	 * \tparam Types types of tuple
	 * \tparam Transform Can be a function or functions in tuple(must be same size with data tuple)
	 * \param tpl tuple
	 * \param _ Ts deduction
	 * \return values in tuple
	 */
	template<typename... Ts, typename... Types, typename Transform,
		typename = std::enable_if_t<TypeArray<Types...>().can_distribute(TypeArray<Ts...>())>>
		[[nodiscard]] constexpr auto pick(std::tuple<Types...>&& tpl, Transform&& transform, TypeArray<Ts...> = {}) noexcept {
		return pick(move(tpl), std::forward<Transform>(transform), TypeArray<Types...>().distribute(TypeArray<Ts...>()));
	}

	/**
	 * \brief Pick Ts values with transform in tuple. Same Types will pick in order.
	 * \tparam Ts
	 * \tparam Types types of tuple
	 * \tparam Transform Can be a function or functions in tuple(must be same size with data tuple)
	 * \param tpl tuple
	 * \param _ Ts deduction
	 * \return values in tuple
	 */
	template<typename... Ts, typename... Types, typename Transform,
		typename = std::enable_if_t<TypeArray<Types...>().can_distribute(TypeArray<Ts...>())>>
		[[nodiscard]] constexpr auto pick(const std::tuple<Types...>&& tpl, Transform&& transform, TypeArray<Ts...> = {}) noexcept {
		return pick(move(tpl), std::forward<Transform>(transform), TypeArray<Types...>().distribute(TypeArray<Ts...>()));
	}
}

#endif
