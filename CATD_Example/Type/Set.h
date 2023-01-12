/*****************************************************************//**
 * \file   Set.h
 * \brief  Set operations of Types.
 *
 * \author YYYCZ
 * \date   December 2022
 *********************************************************************/

#ifndef __YTYPE_SET_H__
#define __YTYPE_SET_H__

#include "Array.h"

namespace ytype {
	/**
	 * \brief Push front Val to integer_sequence<T> {Vals...} if Test is true, or keep {Vals...}.
	 * \tparam Test
	 * \tparam T
	 * \tparam Val
	 * \tparam Vals
	 * \param seq vals deduction
	 * \return new integer_sequence<T> {Val, Vals...} or {Vals...}
	 */
	template<bool Test, typename T, T Val, T... Vals>
	static constexpr auto _Choice_to_push_front_is(std::integer_sequence<T, Vals...> seq = {}) noexcept {
		if constexpr (Test) {
			return std::integer_sequence<T, Val, Vals...>{};
		}
		else {
			return seq;
		}
	}

	/**
	 * \brief Push front T to Ts if Test is true, or keep Ts.
	 * \tparam Test
	 * \tparam T
	 * \tparam Ts
	 * \param ta Ts deduction
	 * \return new TypeArray {T, Ts...} or {Ts...}
	 */
	template<bool Test, typename T, typename... Ts>
	static constexpr auto _Choice_to_push_front(TypeArray<Ts...> ta = {}) noexcept {
		if constexpr (Test) {
			return ta.push_front<T>();
		}
		else {
			return ta;
		}
	}

	/**
	 * \brief Assume TypeArray<Ts...> is a set, add T to set.
	 * \tparam T
	 * \tparam Ts
	 * \param ta Ts deduction
	 * \return new TypeArray {T, Ts...} or {Ts...}
	 */
	template<typename T, typename... Ts>
	static constexpr auto _Add_to_set(TypeArray<Ts...> ta = {}) noexcept {
		static constexpr bool test = !ta.contains<T>();
		return _Choice_to_push_front<test, T>(ta);
	}

	/**
	 * \brief Assitant type to convert TypeArray to a set.
	 * \tparam Types
	 */
	template<typename... Types>
	struct _Subset {
		using type = TypeArray<>;
	};

	template<typename T, typename... Types>
	struct _Subset<T, Types...> {
		using type_array = typename _Subset<Types...>::type;
		using type = decltype(_Add_to_set<T>(type_array{}));
	};

	/**
	 * \brief Convert types to a set array.
	 * \tparam Types
	 * \return a set
	 */
	template<typename... Types>
	using _Convert_to_set_array = typename _Subset<Types...>::type;

	/**
	 * \brief Assume TypeArray<Ts...> is a set,
	 *        add T to set if T is in TypeArray<Ps...> and TypeArray<Qs...>.
	 * \tparam T
	 * \tparam Ts
	 * \tparam Ps
	 * \tparam Qs
	 * \param ta TypeArray<Ts...>
	 * \param p TypeArray<Ps...>
	 * \param q TypeArray<Qs...>
	 * \return new TypeArray {T, Ts...} or {Ts...}
	 */
	template<typename T, typename... Ts, typename... Ps, typename... Qs>
	static constexpr auto _Add_to_intersection_set
	(TypeArray<Ts...> ta = {}, TypeArray<Ps...> p = {}, TypeArray<Qs...> q = {}) noexcept {
		static constexpr bool test = !ta.contains<T>() && p.contains<T>() && q.contains<T>();
		return _Choice_to_push_front<test, T>(ta);
	}

	/**
	 * \brief Assitant type to create intersection set.
	 * \tparam P
	 * \tparam Q
	 * \tparam Types
	 */
	template<typename P, typename Q, typename... Types>
	struct _Intersection_set {
		using type = TypeArray<>;
	};

	template<typename... Ps, typename... Qs, typename T, typename... Types>
	struct _Intersection_set<TypeArray<Ps...>, TypeArray<Qs...>, T, Types...> {
		using p_array = TypeArray<Ps...>;
		using q_array = TypeArray<Qs...>;
		using type_array = typename _Intersection_set<TypeArray<Ps...>, TypeArray<Qs...>, Types...>::type;
		using type = decltype(_Add_to_intersection_set<T>(type_array{}, p_array{}, q_array{}));
	};

	/**
	 * \brief Create intersection of P and Q.
	 * \tparam P
	 * \tparam Q
	 * \tparam Types all types of P and Q
	 * \return a set
	 */
	template<typename P, typename Q, typename... Types>
	using _Create_intersection_set = typename _Intersection_set<P, Q, Types...>::type;

	/**
	 * \brief Assume TypeArray<Ts...> is a set,
	 *        add T to set if T is only in one of TypeArray<Ps...> and TypeArray<Qs...>.
	 * \tparam T
	 * \tparam Ts
	 * \tparam Ps
	 * \tparam Qs
	 * \param ta TypeArray<Ts...>
	 * \param p TypeArray<Ps...>
	 * \param q TypeArray<Qs...>
	 * \return new TypeArray {T, Ts...} or {Ts...}
	 */
	template<typename T, typename... Ts, typename... Ps, typename... Qs>
	static constexpr auto _Add_to_symmetric_difference_set
	(TypeArray<Ts...> ta = {}, TypeArray<Ps...> p = {}, TypeArray<Qs...> q = {}) noexcept {
		static constexpr bool test = !ta.contains<T>() && (p.contains<T>() ^ q.contains<T>());
		return _Choice_to_push_front<test, T>(ta);
	}

	/**
	 * \brief Assitant type to create symmetric difference set.
	 * \tparam P
	 * \tparam Q
	 * \tparam Types
	 */
	template<typename P, typename Q, typename... Types>
	struct _Symmetric_difference_set {
		using type = TypeArray<>;
	};

	template<typename... Ps, typename... Qs, typename T, typename... Types>
	struct _Symmetric_difference_set<TypeArray<Ps...>, TypeArray<Qs...>, T, Types...> {
		using p_array = TypeArray<Ps...>;
		using q_array = TypeArray<Qs...>;
		using type_array = typename _Symmetric_difference_set<TypeArray<Ps...>, TypeArray<Qs...>, Types...>::type;
		using type = decltype(_Add_to_symmetric_difference_set<T>(type_array{}, p_array{}, q_array{}));
	};

	/**
	 * \brief Create symmetric difference of P and Q.
	 * \tparam P
	 * \tparam Q
	 * \tparam Types all types of P and Q
	 * \return a set
	 */
	template<typename P, typename Q, typename... Types>
	using _Create_symmetric_difference_set = typename _Symmetric_difference_set<P, Q, Types...>::type;

	/**
	 * \brief Add T to TypeArray<Ts...> if T is in TypeArray<Ps...>.
	 * \tparam Rev if true, get in P. if false, get not in P
	 * \tparam T
	 * \tparam Ts
	 * \tparam Ps
	 * \param ta TypeArray<Ts...>
	 * \param p TypeArray<Ps...>
	 * \return new TypeArray {T, Ts...} or {Ts...}
	 */
	template<bool Rev, typename T, typename... Ts, typename... Ps>
	static constexpr auto _Add_to_array_if_in(TypeArray<Ts...> ta = {}, TypeArray<Ps...> p = {}) noexcept {
		static constexpr bool test = Rev ^ p.contains<T>();
		return _Choice_to_push_front<test, T>(ta);
	}

	/**
	 * \brief Assitant type to make array whose types are all in set P.
	 * \tparam Rev if true, get in P. if false, get not in P
	 * \tparam P
	 * \tparam Types
	 */
	template<bool Rev, typename P, typename... Types>
	struct _Add_if_in_set {
		using type = TypeArray<>;
	};

	template<bool Rev, typename... Ps, typename T, typename... Types>
	struct _Add_if_in_set<Rev, TypeArray<Ps...>, T, Types...> {
		using p_array = TypeArray<Ps...>;
		using type_array = typename _Add_if_in_set<Rev, TypeArray<Ps...>, Types...>::type;
		using type = decltype(_Add_to_array_if_in<Rev, T>(type_array{}, p_array{}));
	};

	/**
	 * \brief Create an array whose types are all in set P.
	 * \tparam Rev if true, get in P. if false, get not in P
	 * \tparam P
	 * \tparam Types
	 */
	template<bool Rev, typename P, typename... Types>
	using _Create_array_which_in_p = typename _Add_if_in_set<Rev, P, Types...>::type;

	/**
	 * \brief Add N to integer_sequence<I> {Ns...} if T is in P.
	 * \tparam Rev if true, add in P. if false, add not in P
	 * \tparam T
	 * \tparam I
	 * \tparam N
	 * \tparam Ns
	 * \tparam Ps
	 * \param seq integer_sequence<I, Ns...>
	 * \param p TypeArray<Ps...>
	 * \return new integer_sequence<I> {N, Ns...} or {Ns...}
	 */
	template<bool Rev, typename T, typename I, I N, I... Ns, typename... Ps>
	static constexpr auto _Get_indexes_in_set(std::integer_sequence<I, Ns...> seq = {}, TypeArray<Ps...> p = {}) noexcept {
		static constexpr bool test = Rev ^ p.contains<T>();
		return _Choice_to_push_front_is<test, I, N>(seq);
	}

	/**
	 * \brief Assitant type to make indexes whose types are all in set P.
	 * \tparam Rev if true, get in P. if false, get not in P
	 * \tparam N the offset
	 * \tparam P
	 * \tparam Types
	 */
	template<bool Rev, size_t N, typename P, typename... Types>
	struct _Add_index_if_in_set {
		using type = std::integer_sequence<size_t>;
	};

	template<bool Rev, size_t N, typename... Ps, typename T, typename... Types>
	struct _Add_index_if_in_set<Rev, N, TypeArray<Ps...>, T, Types...> {
		using p_array = TypeArray<Ps...>;
		using index_array = typename _Add_index_if_in_set<Rev, N + 1, TypeArray<Ps...>, Types...>::type;
		using type = decltype(_Get_indexes_in_set<Rev, T, size_t, N>(index_array{}, p_array{}));
	};

	/**
	 * \brief Create an indexes whose types are all in set P.
	 * \tparam Rev if true, get in P. if false, get not in P
	 * \tparam P
	 * \tparam Types
	 */
	template<bool Rev, typename P, typename... Types>
	using _Create_indexes_which_in_p = typename _Add_index_if_in_set<Rev, 0, P, Types...>::type;
}

namespace ytype {
	/**
	 * \brief Get the empty type set.
	 * \return the empty type set
	 */
	static constexpr TypeArray<> get_empty_type_set() noexcept { return {}; }

	/**
	 * \brief The empty type set.
	 */
	static constexpr auto ets = get_empty_type_set();

	/**
	 * \brief Create a set by TypeArray.
	 * \tparam T
	 * \tparam Types
	 * \param _ T deduction
	 * \param _ types deduction
	 * \return a set array
	 */
	template<typename T, typename... Types>
	static constexpr auto create_type_set(T, Types...) noexcept {
		return _Convert_to_set_array<T, Types...>{};
	}

	/**
	 * \brief Create a set by TypeArray.
	 * \tparam Types
	 * \param _ types deduction
	 * \return a set array
	 */
	template<typename... Types>
	static constexpr auto create_type_set(TypeArray<Types...> _ = {}) noexcept {
		return _Convert_to_set_array<Types...>{};
	}

	/**
	 * \brief Create a set by TypeArray.
	 * \tparam Types
	 * \param _ types deduction
	 * \return a set array
	 */
	template<typename... Types>
	static constexpr auto operator~(TypeArray<Types...> _) noexcept {
		return _Convert_to_set_array<Types...>{};
	}

	/**
	 * \brief Check if TypeArray a contains TypeArray b in set meaning.
	 * \tparam Ts
	 * \tparam Ps
	 * \param a TypeArray a
	 * \param b TypeArray b
	 * \return if is contains
	 */
	template<typename... Ts, typename... Ps>
	static constexpr auto type_set_contains(TypeArray<Ts...> a = {}, TypeArray<Ps...> b = {}) noexcept {
		return (a.contains<Ps>() && ...);
	}

	/**
	 * \brief Check if TypeArray a contains TypeArray b in set meaning.
	 * \tparam Ts
	 * \tparam Ps
	 * \param a TypeArray a
	 * \param b TypeArray b
	 * \return if is contains
	 */
	template<typename... Ts, typename... Ps>
	static constexpr bool operator>=(TypeArray<Ts...> a, TypeArray<Ps...> b) noexcept {
		return (a.contains<Ps>() && ...);
	}

	/**
	 * \brief Check if TypeArray a is a subset of TypeArray b.
	 * \tparam Ts
	 * \tparam Ps
	 * \param a TypeArray a
	 * \param b TypeArray b
	 * \return if is subset
	 */
	template<typename... Ts, typename... Ps>
	static constexpr auto type_set_subset(TypeArray<Ts...> a = {}, TypeArray<Ps...> b = {}) noexcept {
		return (b.contains<Ts>() && ...);
	}

	/**
	 * \brief Check if TypeArray a is a subset of TypeArray b.
	 * \tparam Ts
	 * \tparam Ps
	 * \param a TypeArray a
	 * \param b TypeArray b
	 * \return if is contains
	 */
	template<typename... Ts, typename... Ps>
	static constexpr bool operator<=(TypeArray<Ts...> a, TypeArray<Ps...> b) noexcept {
		return (b.contains<Ts>() && ...);
	}

	/**
	 * \brief Check if TypeArray a is a proper subset of TypeArray b.
	 * \tparam Ts
	 * \tparam Ps
	 * \param a TypeArray a
	 * \param b TypeArray b
	 * \return if is proper subset
	 */
	template<typename... Ts, typename... Ps>
	static constexpr auto type_set_proper_subset(TypeArray<Ts...> a = {}, TypeArray<Ps...> b = {}) noexcept {
		return (b.contains<Ts>() && ...) && ((!a.contains<Ps>()) || ...);
	}

	/**
	 * \brief Check if TypeArray a is an unordered subarray of TypeArray b.
	 * \tparam Ts
	 * \tparam Ps
	 * \param a TypeArray a
	 * \param b TypeArray b
	 * \return if is subarray
	 */
	template<typename... Ts, typename... Ps>
	static constexpr auto is_sub_type_array(TypeArray<Ts...> a = {}, TypeArray<Ps...> b = {}) noexcept {
		return ((b.count<Ts>() >= a.count<Ts>()) && ...);
	}

	/**
	 * \brief Check if TypeArray a is an unordered subarray of TypeArray b.
	 * \tparam Ts
	 * \tparam Ps
	 * \param a TypeArray a
	 * \param b TypeArray b
	 * \return if is subarray
	 */
	template<typename... Ts, typename... Ps>
	static constexpr auto operator<(TypeArray<Ts...> a, TypeArray<Ps...> b) noexcept {
		return ((b.count<Ts>() >= a.count<Ts>()) && ...);
	}

	/**
	 * \brief Check if TypeArray b is an unordered subarray of TypeArray a.
	 * \tparam Ts
	 * \tparam Ps
	 * \param a TypeArray a
	 * \param b TypeArray b
	 * \return if is subarray
	 */
	template<typename... Ts, typename... Ps>
	static constexpr auto operator>(TypeArray<Ts...> a, TypeArray<Ps...> b) noexcept {
		return ((a.count<Ps>() >= b.count<Ps>()) && ...);
	}

	/**
	 * \brief Check if TypeArray a and b is equal in set meaning.
	 * \tparam Ts
	 * \tparam Ps
	 * \param a TypeArray a
	 * \param b TypeArray b
	 * \return if is equal
	 */
	template<typename... Ts, typename... Ps>
	static constexpr auto type_set_equal(TypeArray<Ts...> a = {}, TypeArray<Ps...> b = {}) noexcept {
		return (a >= b) && (a <= b);
	}

	/**
	 * \brief Check if TypeArray a and b has total same types(unordered).
	 * \tparam Ts
	 * \tparam Ps
	 * \param a TypeArray a
	 * \param b TypeArray b
	 * \return if is equal
	 */
	template<typename... Ts, typename... Ps>
	static constexpr auto has_same_types(TypeArray<Ts...> a = {}, TypeArray<Ps...> b = {}) noexcept {
		return (a > b) && (a < b);
	}

	/**
	 * \brief Check if TypeArray a and b is equal in unordered array meaning.
	 * \tparam Ts
	 * \tparam Ps
	 * \param a TypeArray a
	 * \param b TypeArray b
	 * \return if is equal
	 */
	template<typename... Ts, typename... Ps>
	static constexpr auto operator==(TypeArray<Ts...> a, TypeArray<Ps...> b) noexcept {
		return (a > b) && (a < b);
	}

	/**
	 * \brief Check if TypeArray a and b is unequal in unordered array meaning.
	 * \tparam Ts
	 * \tparam Ps
	 * \param a TypeArray a
	 * \param b TypeArray b
	 * \return if is unequal
	 */
	template<typename... Ts, typename... Ps>
	static constexpr auto operator!=(TypeArray<Ts...> a, TypeArray<Ps...> b) noexcept {
		return !(a == b);
	}

	/**
	 * \brief Get the union set of TypeArray a and b.
	 * \tparam Ts
	 * \tparam Ps
	 * \param a TypeArray a
	 * \param b TypeArray b
	 * \return the union set
	 */
	template<typename... Ts, typename... Ps>
	static constexpr auto type_set_union(TypeArray<Ts...> a = {}, TypeArray<Ps...> b = {}) noexcept {
		return _Convert_to_set_array<Ts..., Ps...>{};
	}

	/**
	 * \brief Get the union set of TypeArray a and b.
	 * \tparam Ts
	 * \tparam Ps
	 * \param a TypeArray a
	 * \param b TypeArray b
	 * \return the union set
	 */
	template<typename... Ts, typename... Ps>
	static constexpr auto operator|(TypeArray<Ts...> a, TypeArray<Ps...> b) noexcept {
		return _Convert_to_set_array<Ts..., Ps...>{};
	}

	/**
	 * \brief Get the intersection set of TypeArray a and b.
	 * \tparam Ts
	 * \tparam Ps
	 * \param a TypeArray a
	 * \param b TypeArray b
	 * \return the intersection set
	 */
	template<typename... Ts, typename... Ps>
	static constexpr auto type_set_intersection(TypeArray<Ts...> a = {}, TypeArray<Ps...> b = {}) noexcept {
		return _Create_intersection_set<TypeArray<Ts...>, TypeArray<Ps...>, Ts..., Ps...>{};
	}

	/**
	 * \brief Get the intersection set of TypeArray a and b.
	 * \tparam Ts
	 * \tparam Ps
	 * \param a TypeArray a
	 * \param b TypeArray b
	 * \return the intersection set
	 */
	template<typename... Ts, typename... Ps>
	static constexpr auto operator&(TypeArray<Ts...> a, TypeArray<Ps...> b) noexcept {
		return _Create_intersection_set<TypeArray<Ts...>, TypeArray<Ps...>, Ts..., Ps...>{};
	}

	/**
	 * \brief Get the symmetric difference set of TypeArray a and b.
	 * \tparam Ts
	 * \tparam Ps
	 * \param a TypeArray a
	 * \param b TypeArray b
	 * \return the symmetric difference set
	 */
	template<typename... Ts, typename... Ps>
	static constexpr auto type_set_symmetric_difference(TypeArray<Ts...> a = {}, TypeArray<Ps...> b = {}) noexcept {
		return _Create_symmetric_difference_set<TypeArray<Ts...>, TypeArray<Ps...>, Ts..., Ps...>{};
	}

	/**
	 * \brief Get the symmetric difference set of TypeArray a and b.
	 * \tparam Ts
	 * \tparam Ps
	 * \param a TypeArray a
	 * \param b TypeArray b
	 * \return the symmetric difference set
	 */
	template<typename... Ts, typename... Ps>
	static constexpr auto operator^(TypeArray<Ts...> a, TypeArray<Ps...> b) noexcept {
		return _Create_symmetric_difference_set<TypeArray<Ts...>, TypeArray<Ps...>, Ts..., Ps...>{};
	}

	/**
	 * \brief Get the difference set of TypeArray a and b.
	 * \tparam Ts
	 * \tparam Ps
	 * \param a TypeArray a
	 * \param b TypeArray b
	 * \return the difference set
	 */
	template<typename... Ts, typename... Ps>
	static constexpr auto type_set_difference(TypeArray<Ts...> a = {}, TypeArray<Ps...> b = {}) noexcept {
		return _Create_symmetric_difference_set<TypeArray<Ts...>, TypeArray<Ps...>, Ts...>{};
	}

	/**
	 * \brief Get the difference set of TypeArray a and b.
	 * \tparam Ts
	 * \tparam Ps
	 * \param a TypeArray a
	 * \param b TypeArray b
	 * \return the difference set
	 */
	template<typename... Ts, typename... Ps>
	static constexpr auto operator-(TypeArray<Ts...> a, TypeArray<Ps...> b) noexcept {
		return _Create_symmetric_difference_set<TypeArray<Ts...>, TypeArray<Ps...>, Ts...>{};
	}

	/**
	 * \brief Pick types of TypeArray<Ts...> which are in TypeArray<Ps...>.
	 * \tparam Ts
	 * \tparam Ps
	 * \param a TypeArray a
	 * \param b TypeArray b
	 * \return the picked TypeArray
	 */
	template<typename... Ts, typename... Ps>
	static constexpr auto pick_types_in_set(TypeArray<Ts...> a = {}, TypeArray<Ps...> b = {}) noexcept {
		return _Create_array_which_in_p<false, TypeArray<Ps...>, Ts...>{};
	}

	/**
	 * \brief Pick types of TypeArray<Ts...> which are in TypeArray<Ps...>.
	 * \tparam Ts
	 * \tparam Ps
	 * \param a TypeArray a
	 * \param b TypeArray b
	 * \return the picked TypeArray
	 */
	template<typename... Ts, typename... Ps>
	static constexpr auto operator%(TypeArray<Ts...> a, TypeArray<Ps...> b) noexcept {
		return _Create_array_which_in_p<false, TypeArray<Ps...>, Ts...>{};
	}

	/**
	 * \brief Pick types of TypeArray<Ts...> which are not in TypeArray<Ps...>.
	 * \tparam Ts
	 * \tparam Ps
	 * \param a TypeArray a
	 * \param b TypeArray b
	 * \return the picked TypeArray
	 */
	template<typename... Ts, typename... Ps>
	static constexpr auto pick_types_not_in_set(TypeArray<Ts...> a = {}, TypeArray<Ps...> b = {}) noexcept {
		return _Create_array_which_in_p<true, TypeArray<Ps...>, Ts...>{};
	}

	/**
	 * \brief Pick types of TypeArray<Ts...> which are not in TypeArray<Ps...>.
	 * \tparam Ts
	 * \tparam Ps
	 * \param a TypeArray a
	 * \param b TypeArray b
	 * \return the picked TypeArray
	 */
	template<typename... Ts, typename... Ps>
	static constexpr auto operator/(TypeArray<Ts...> a, TypeArray<Ps...> b) noexcept {
		return _Create_array_which_in_p<true, TypeArray<Ps...>, Ts...>{};
	}

	/**
	 * \brief Find all types which are in TypeArray<Ts...>.
	 * \tparam Ts
	 * \param TypeArray<Ts...>
	 * \return the indexes
	 */
	template<typename... Types>
	template<typename... Ts>
	static constexpr auto TypeArray<Types...>::find_all_in(TypeArray<Ts...>) noexcept {
		return _Create_indexes_which_in_p<false, TypeArray<Ts...>, Types...>{};
	}

	/**
	 * \brief Find all types which are not in TypeArray<Ts...>.
	 * \tparam Ts
	 * \param TypeArray<Ts...>
	 * \return the indexes
	 */
	template<typename... Types>
	template<typename... Ts>
	static constexpr auto TypeArray<Types...>::find_all_not_in(TypeArray<Ts...>) noexcept {
		return _Create_indexes_which_in_p<true, TypeArray<Ts...>, Types...>{};
	}

	/**
	 * \brief Check if TypeArray<Types...> is minimum set array of self.
	 * \return if is set
	 */
	template<typename... Types>
	inline constexpr bool TypeArray<Types...>::is_set() noexcept {
		return TypeArray<Types...>{} < create_type_set<Types...>();
	}
}

#endif
