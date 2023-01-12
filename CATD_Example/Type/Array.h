/*****************************************************************//**
 * \file   Array.h
 * \brief  Some utils to deal with the types pack like "array".
 *
 * \author YYYCZ
 * \date   December 2022
 *********************************************************************/

#ifndef __YTYPE_ARRAY_H__
#define __YTYPE_ARRAY_H__

#include <inttypes.h>
#include "Ordinary.h"

namespace ytype {
	/**
	 * \brief The array to store types.
	 * \tparam Types
	 */
	template<typename... Types>
	class TypeArray;

	/**
	 * \brief Create TypeArray<Types...> by types deduction.
	 * \tparam Types
	 * \param _ types deduction
	 * \return TypeArray<Types...>
	 */
	template<typename... Types>
	static constexpr auto create_type_array(Types...) noexcept {
		return TypeArray<Types...>{};
	}

	/**
	 * \brief Concat 2 TypeArray.
	 * \tparam Ps
	 * \tparam Qs
	 * \param p TypeArray{Ps...}
	 * \param q TypeArray{Qs...}
	 * \return TypeArray{Ps..., Qs...}
	 */
	template<typename... Ps, typename... Qs>
	static constexpr auto operator+(TypeArray<Ps...> p, TypeArray<Qs...> q) noexcept {
		return TypeArray<Ps..., Qs...>{};
	}
}

namespace ytype {
	/**
	 * \brief Remove const and referecen of T.
	 * \tparam T
	 * \return new type
	 */
	template<typename T>
	using remove_cr_t = std::remove_const_t<std::remove_reference_t<T>>;

	/**
	 * \brief Move cref of From to cref of To.
	 * \tparam From
	 * \tparam To
	 * \return cref type in Wrapper
	 */
	template<typename From, typename To>
	static constexpr auto _Move_cref_to() noexcept {
		static constexpr bool is_lref = std::is_lvalue_reference_v<From>;
		static constexpr bool is_rref = std::is_rvalue_reference_v<From>;
		static constexpr bool not_ref = !is_lref && !is_rref;
		static constexpr bool ref_const = std::is_const_v<From>;
		static constexpr bool not_const = !std::is_const_v<std::remove_reference_t<From>>;

		if constexpr (not_const && not_ref) {
			return Wrapper<To>{};
		}
		else if constexpr (!not_const && not_ref) {
			return Wrapper<const To>{};
		}
		else if constexpr (not_const && is_lref) {
			return Wrapper<To&>{};
		}
		else if constexpr (not_const && is_rref) {
			return Wrapper<To&&>{};
		}
		else if constexpr (ref_const && is_lref) {
			return Wrapper<To const&>{};
		}
		else if constexpr (ref_const && is_rref) {
			return Wrapper<To const&&>{};
		}
		else if constexpr (!not_const && is_lref) {
			return Wrapper<const To&>{};
		}
		else if constexpr (!not_const && is_rref) {
			return Wrapper<const To&&>{};
		}
		else {
			return Wrapper<To>{};
		}
	}

	/**
	 * \brief Move the const and reference type of From to To.
	 * \tparam From
	 * \tparam To
	 * \tparam new type
	 */
	template<typename From, typename To>
	using move_cr_to = getWrapType<decltype(_Move_cref_to<From, To>())>;

	/**
	 * \brief Assitant type to unwrap types in TypeArray.
	 * \tparam Types types in TypeArray
	 */
	template<typename... Types>
	struct _Unwrap_to_type_array { using type = TypeArray<getWrapType<Types>...>; };

	template<typename... Types>
	struct _Unwrap_to_type_array<TypeArray<Types...>> { using type = TypeArray<getWrapType<Types>...>; };

	/**
	 * \brief Unwrap types to TypeArray.
	 * \tparam Types
	 * \return TypeArray with unwrapped type
	 */
	template<typename... Types>
	using unwrapToTypeArray = typename _Unwrap_to_type_array<Types...>::type;

	/**
	 * \brief If x and y are positive, return x + y, or return -1.
	 * \tparam T
	 * \param x
	 * \param y
	 * \return x + y or -1
	 */
	template<typename T>
	static constexpr T _Index_add(T x, T y) {
		if (x < 0 || y < 0) return T(-1);
		return x + y;
	}

	/**
	 * \brief If x and val in vals are positive, return {x + vals ...}, or {-1, ...}.
	 * \tparam T
	 * \tparam Val
	 * \tparam Vals
	 * \param _ vals deduction
	 * \return {x + vals ...} or {-1, ...}
	 */
	template<typename T, T Val, T... Vals>
	static constexpr auto _Indexes_add(std::integer_sequence<T, Vals...>) {
		return std::integer_sequence<T, _Index_add(Val, Vals)...>{};
	}

	/**
	 * \brief Append val to T type's integer_sequence vals.
	 * \tparam T
	 * \tparam Val
	 * \tparam Vals
	 * \param _ vals deduction
	 * \return T type's integer_sequence {val, vals...}
	 */
	template<typename T, T Val, T... Vals>
	static constexpr auto append_to_integer_sequence(std::integer_sequence<T, Vals...>) {
		return std::integer_sequence<T, Val, Vals...>{};
	}

	/**
	 * \brief Convert one type integer_sequence to another type.
	 * \tparam T target type of integer_sequence
	 * \tparam P original type of integer_sequence
	 * \param _ vals deduction
	 * \return T type's integer_sequence {vals...}
	 */
	template<typename T, typename P, P... Vals>
	static constexpr auto convert_integer_sequence(std::integer_sequence<P, Vals...>) {
		return std::integer_sequence<T, static_cast<T>(Vals)...>{};
	}

	/**
	 * \brief Assitant type to get Nth type in types.
	 * \tparam N
	 * \tparam Types
	 */
	template<size_t N, typename... Types>
	class _Type_Nth {
	private:
		template<typename T>
		static constexpr T* _Get_first_type(T*, ...) { return nullptr; }

		template<typename... Fs, typename T>
		static constexpr T* _Reduce_forward_to_single(Fs..., T*, ...) { return nullptr; };

		template<size_t... Ns>
		static constexpr auto _Get_one_type_forward(std::index_sequence<Ns...>) {
			return *_Reduce_forward_to_single<decltype((void*)Ns)...>(reinterpret_cast<Wrapper<Types>*>(0)...);
		}

		static constexpr auto getNType() noexcept {
			if constexpr (sizeof...(Types) > N) {
				if constexpr (N == 0) {
					return *_Get_first_type(reinterpret_cast<Wrapper<Types>*>(0)...);
				}
				else {
					return _Get_one_type_forward(std::make_index_sequence<N>());
				}
			}
			else {
				return NullType{};
			}
		}

	public:
		using type = getWrapType<decltype(getNType())>;
	};

	/**
	 * \brief Assitant type to pop front N types.
	 * \tparam N
	 * \tparam Types
	 */
	template<size_t N, typename... Types>
	class _Types_pop_front {
	private:
		template<typename... Fs, typename... Ts>
		static constexpr TypeArray<Ts...>* _Reduce_forward(Fs..., Ts*...) { return nullptr; }

		template<size_t... Ns>
		static constexpr auto _Reduce_types_forward(std::index_sequence<Ns...>) {
			return *_Reduce_forward<decltype((void*)Ns)...>(reinterpret_cast<Wrapper<Types>*>(0)...);
		}

		static constexpr auto getNType() noexcept {
			if constexpr (sizeof...(Types) > N) {
				return unwrapToTypeArray<decltype(_Reduce_types_forward(std::make_index_sequence<N>()))>{};
			}
			else {
				return TypeArray<>{};
			}
		}

	public:
		using type = decltype(getNType());
	};

	/**
	 * \brief Merge T to Types in TypeArray.
	 * \tparam T
	 * \tparam Types
	 */
	template<typename T, typename... Types>
	struct _Merge_type_array { using type = TypeArray<T, Types...>; };

	template<typename T, typename... Types>
	struct _Merge_type_array<T, TypeArray<Types...>> { using type = TypeArray<T, Types...>; };

	/**
	 * \brief Take N types forward.
	 * \tparam N
	 * \tparam Types
	 */
	template<size_t N, typename... Types>
	struct _Take_types_forward;

	template<size_t N>
	struct _Take_types_forward<N> {
		using type = TypeArray<>;
	};

	template<size_t N, typename T, typename... Types>
	struct _Take_types_forward<N, T, Types...> {
		static constexpr auto* _Take() noexcept {
			if constexpr (N) {
				using P = typename _Take_types_forward<N - 1, Types...>::type;
				using Q = typename _Merge_type_array<T, P>::type;
				return reinterpret_cast<Q*>(0);
			}
			else {
				return reinterpret_cast<TypeArray<>*>(0);
			}
		}

		using type = std::remove_pointer_t<decltype(_Take())>;
	};

	/**
	 * \brief Assitant type to pop back N types.
	 * \tparam N
	 * \tparam Types
	 */
	template<size_t N, typename... Types>
	class _Types_pop_back {
	private:
		static constexpr auto getNType() noexcept {
			if constexpr (sizeof...(Types) > N) {
				using Ret = typename _Take_types_forward<sizeof...(Types) - N, Types...>::type;
				return Ret{};
			}
			else {
				return TypeArray<>{};
			}
		}

	public:
		using type = decltype(getNType());
	};

	/**
	 * \brief Get the Nth type in types.
	 * \tparam N
	 * \tparam Types
	 * \return the Nth type
	 */
	template<size_t N, typename... Types>
	using NthType = typename _Type_Nth<N, Types...>::type;

	/**
	 * \brief Pop front N types.
	 * \tparam N
	 * \tparam Types
	 * \return the types left in TypeArray
	 */
	template<size_t N, typename... Types>
	using PopFrontTypes = typename _Types_pop_front<N, Types...>::type;

	/**
	 * \brief Pop back N types.
	 * \tparam N
	 * \tparam Types
	 * \return the types left in TypeArray
	 */
	template<size_t N, typename... Types>
	using PopBackTypes = typename _Types_pop_back<N, Types...>::type;

	/**
	 * \brief Assitant variable to find type in types.
	 * \tparam T the type be sought
	 * \tparam I the index
	 * \tparam Types
	 * \return the index of type
	 */
	template<typename T, size_t I, typename... Types>
	static constexpr int _Type_index;

	template<typename T, size_t I>
	static constexpr int _Type_index<T, I> = -1;

	template<typename T, size_t I, typename... Types>
	static constexpr int _Type_index<T, I, T, Types...> = I;

	template<typename T, size_t I, typename P, typename... Types>
	static constexpr int _Type_index<T, I, P, Types...> = _Type_index<T, I + 1, Types...>;

	/**
	 * \brief Get the Index of T in Types. If not found, get -1.
	 * \tparam T the type be sought
	 * \tparam Types
	 * \return the index of type
	 */
	template<typename T, typename... Types>
	static constexpr int TypeIndex = _Type_index<T, 0, Types...>;

	/**
	 * \brief Get the Nth type's index in types. If not found, get -1.
	 * \tparam N
	 * \tparam T the type be sought
	 * \return the type index
	 */
	template<size_t N, typename T, typename... Types>
	static constexpr int NthTypeIndex;

	template<size_t N, typename T>
	static constexpr int NthTypeIndex<N, T> = -1;

	template<size_t N, typename T, typename... Types>
	static constexpr int NthTypeIndex<N, T, T, Types...> = N == 0 ?
		TypeIndex<T, T, Types...> : _Index_add(1, NthTypeIndex<N - 1, T, Types...>);

	template<size_t N, typename T, typename P, typename... Types>
	static constexpr int NthTypeIndex<N, T, P, Types...> = N == 0 ?
		TypeIndex<T, P, Types...> : _Index_add(1, NthTypeIndex<N, T, Types...>);

	/**
	 * \brief Count the appear times of T in types.
	 * \tparam T
	 * \tparam Types
	 * \return appear times
	 */
	template<typename T, typename... Types>
	static constexpr size_t TypeCount;

	template<typename T>
	static constexpr size_t TypeCount<T> = 0;

	template<typename T, typename... Types>
	static constexpr size_t TypeCount<T, T, Types...> = 1 + TypeCount<T, Types...>;

	template<typename T, typename P, typename... Types>
	static constexpr size_t TypeCount<T, P, Types...> = TypeCount<T, Types...>;
}

namespace ytype {
	/**
	 * \brief Check if T is TypeArray.
	 * \tparam T
	 */
	template<typename T>
	struct _Is_type_array :public std::false_type {};

	template<typename... Types>
	struct _Is_type_array<TypeArray<Types...>> :public std::true_type {};

	/**
	 * \brief Check if T is TypeArray.
	 * \tparam T
	 * \return if is TypeArray
	 */
	template<typename T>
	static constexpr bool isTypeArray() noexcept {
		return _Is_type_array<T>::value;
	}

	/**
	 * \brief Check if T is TypeArray.
	 * \tparam T
	 * \return if is TypeArray
	 */
	template<typename T>
	static constexpr bool isTypeArray(T) noexcept {
		return _Is_type_array<T>::value;
	}

	/**
	 * \brief The array to store types.\n
	 *		  Node: Some implements of methods are not in "THIS FILE".
	 *		  Need to include other headers as introduction.
	 * \tparam Types
	 */
	template<typename... Types>
	class TypeArray {
	public:
		/**
		 * \brief Default constructable.
		 */
		TypeArray() = default;

		/**
		 * \brief The size of TypeArray.
		 * \return the size
		 */
		static constexpr size_t size() noexcept {
			return sizeof...(Types);
		}

		/**
		 * \brief Check if the TypeArray is empty.
		 * \return if is empty
		 */
		static constexpr bool empty() noexcept {
			return size() == 0;
		}

		/**
		 * \brief Check if T is same to self.
		 * \tparam T
		 * \return if is same
		 */
		template<typename T>
		static constexpr bool same() noexcept {
			return std::is_same_v<T, TypeArray<Types...>>;
		}

		/**
		 * \brief Check if T is same to self.
		 * \tparam T
		 * \param T deduction
		 * \return if is same
		 */
		template<typename T>
		static constexpr bool same(T) noexcept {
			return std::is_same_v<T, TypeArray<Types...>>;
		}

		/**
		 * \brief Get the Nth type in types.
		 * \tparam N
		 * \return the Nth type
		 */
		template<size_t N>
		static constexpr NthType<N, Types...> at() noexcept { return {}; }

		/**
		 * @brief Get the front type in types.
		 * @return front type
		 */
		static constexpr NthType<0, Types...> front() noexcept { return {}; }

		/**
		 * @brief Get the back type in types.
		 * @return back type
		 */
		static constexpr NthType<sizeof...(Types) - 1, Types...> back() noexcept { return {}; }

		/**
		 * \brief Pick the types of Ns indexes to create a new TypeArray.
		 * \tparam Ns
		 * \param _ Ns deduction
		 * \return new TypeArray
		 */
		template<size_t... Ns>
		static constexpr auto pick(std::index_sequence<Ns...> _ = {}) noexcept {
			return TypeArray<NthType<Ns, Types...>...>{};
		}

		/**
		 * \brief Push Ts to the front of types.
		 * \tparam Ts deduction
		 * \return new TypeArray
		 */
		template<typename... Ts>
		static constexpr TypeArray<Ts..., Types...> push_front(TypeArray<Ts...> _ = {}) noexcept { return {}; }

		/**
		 * \brief Push Ts to the back of types.
		 * \tparam Ts deduction
		 * \return new TypeArray
		 */
		template<typename... Ts>
		static constexpr TypeArray<Types..., Ts...> push_back(TypeArray<Ts...> _ = {}) noexcept { return {}; }

		/**
		 * \brief Pop N front types.
		 * \tparam N
		 * \return the types left in TypeArray
		 */
		template<size_t N>
		static constexpr PopFrontTypes<N, Types...> pop_front() noexcept { return {}; }

		/**
		 * \brief Pop back N types.
		 * \tparam N
		 * \return the types left in TypeArray
		 */
		template<size_t N>
		static constexpr PopBackTypes<N, Types...> pop_back() noexcept { return {}; }

		/**
		 * \brief Take N types.
		 * \tparam N
		 * \return the types be take
		 */
		template<size_t N>
		static constexpr PopBackTypes<(sizeof...(Types) < N) ? 0 : sizeof...(Types) - N, Types...>
			take() noexcept { return {}; }

		/**
		 * \brief Drop N types.
		 * \tparam N
		 * \return the types left in TypeArray
		 */
		template<size_t N>
		static constexpr auto drop() noexcept {
			return pop_front<N>();
		}

		/**
		 * \brief Get the [L, R) range in TypeArray.
		 * \tparam L
		 * \tparam R
		 * \return new types range
		 */
		template<size_t L, size_t R>
		static constexpr auto range() noexcept {
			return drop<L>().take<R - L>();
		}

		/**
		 * \brief Check if TypeArray contains the T.
		 * \tparam T
		 * \return if contains T
		 */
		template<typename T>
		static constexpr bool contains() noexcept {
			return TypeIndex<T, Types...> != -1;
		}

		/**
		 * \brief Check if TypeArray contains the T.
		 * \tparam T
		 * \param T deduction
		 * \return if contains T
		 */
		template<typename T>
		static constexpr bool contains(T) noexcept {
			return TypeIndex<T, Types...> != -1;
		}

		/**
		 * \brief Check if TypeArray contains the all types in TypeArray<Ts...>.
		 * \tparam Ts
		 * \param _ Ts deduction
		 * \return if contains Ts
		 */
		template<typename... Ts>
		static constexpr bool contains_all(TypeArray<Ts...> _ = {}) noexcept {
			return (contains<Ts>() && ...);
		}

		/**
		 * \brief Check if TypeArray contains the any type in TypeArray<Ts...>.
		 * \tparam Ts
		 * \param _ Ts deduction
		 * \return if contains Ts
		 */
		template<typename... Ts>
		static constexpr bool contains_any(TypeArray<Ts...> _ = {}) noexcept {
			return (contains<Ts>() || ...);
		}

		/**
		 * \brief Find type in types. If not found, get -1.
		 * \tparam T the type be sought
		 * \tparam Pos the start position with 0 as default value
		 * \return the index of type
		 */
		template<typename T, size_t Pos = 0>
		static constexpr int find() noexcept {
			if constexpr (Pos) {
				return _Index_add(Pos, drop<Pos>().find<T>());
			}
			else {
				return TypeIndex<T, Types...>;
			}
		}

		/**
		 * \brief Find type in types. If not found, get -1.
		 * \tparam T the type be sought
		 * \tparam Pos the start position with 0 as default value
		 * \param T deduction
		 * \return the index of type
		 */
		template<typename T, size_t Pos = 0>
		static constexpr int find(T) noexcept {
			return find<T, Pos>();
		}

		/**
		 * \brief Find the Nth type in types start at Pos. If not found, get -1.
		 * \tparam T the type be sought
		 * \tparam N
		 * \tparam Pos the start position
		 * \return the type index
		 */
		template<typename T, size_t N, size_t Pos = 0>
		static constexpr int find_nth() noexcept {
			if constexpr (Pos) {
				return _Index_add(Pos, drop<Pos>().find_nth<T>());
			}
			else {
				return NthTypeIndex<N, T, Types...>;
			}
		}

		/**
		 * \brief Find the Nth type in types start at Pos. If not found, get -1.
		 * \tparam T the type be sought
		 * \tparam N
		 * \tparam Pos the start position
		 * \param T deduction
		 * \return the type index
		 */
		template<typename T, size_t N, size_t Pos = 0>
		static constexpr int find_nth(T) noexcept {
			return find_nth<T, N, Pos>();
		}

		/**
		 * \brief Find all the type in types start at Pos. If not found, get {}.
		 * \tparam T
		 * \tparam Pos
		 * \return the indexes
		 */
		template<typename T, size_t Pos = 0>
		static constexpr auto find_all() noexcept {
			if constexpr (Pos) {
				return _Indexes_add<size_t, Pos>(drop<Pos>().find_all<T>());
			}
			else {
				static constexpr int next_index = find<T>();
				if constexpr (next_index == -1) {
					return std::integer_sequence<size_t>{};
				}
				else {
					return append_to_integer_sequence<size_t, next_index>(find_all<T, next_index + 1>());
				}
			}
		}

		/**
		 * \brief Find all the type in types start at Pos. If not found, get {}.
		 * \tparam T
		 * \tparam Pos
		 * \param T deduction
		 * \return the indexes
		 */
		template<typename T, size_t Pos = 0>
		static constexpr auto find_all(T) noexcept {
			return find_all<T, Pos>();
		}

		/**
		 * \brief Find all types which are in TypeArray<Ts...>.\n
		 *		  Note: Defined in "Set.h".
		 * \tparam Ts
		 * \param TypeArray<Ts...>
		 * \return the indexes
		 */
		template<typename... Ts>
		static constexpr auto find_all_in(TypeArray<Ts...> = {}) noexcept;

		/**
		 * \brief Find all types which are not in TypeArray<Ts...>.\n
		 *        Note: Defined in "Set.h".
		 * \tparam Ts
		 * \param TypeArray<Ts...>
		 * \return the indexes
		 */
		template<typename... Ts>
		static constexpr auto find_all_not_in(TypeArray<Ts...> = {}) noexcept;

		/**
		 * \brief Check if TypeArray<Types...> is minimum set array of self.
		 *		  Note: Defined in "Set.h".
		 * \return if is set
		 */
		static constexpr bool is_set() noexcept;

		/**
		 * \brief Count the appear times of T in types.
		 * \tparam T
		 * \return appear times
		 */
		template<typename T>
		static constexpr size_t count() noexcept {
			return TypeCount<T, Types...>;
		}

		/**
		 * \brief Count the appear times of T in types.
		 * \tparam T
		 * \param T deduction
		 * \return appear times
		 */
		template<typename T>
		static constexpr size_t count(T) noexcept {
			return TypeCount<T, Types...>;
		}

		/**
		 * \brief Count the appear times of Ts in types.
		 * \tparam Ts
		 * \param _ Ts deduction
		 * \return appear times
		 */
		template<typename... Ts>
		static constexpr size_t count(TypeArray<Ts...> _ = {}) noexcept {
			return ((_.contains<Types>() ? (size_t)1 : (size_t)0) + ...);
		}

		/**
		 * \brief Map to if types is in Ts.
		 * \tparam Ts
		 * \param _ Ts deduction
		 * \return integer_sequence<bool>{if is in}
		 */
		template<typename... Ts>
		static constexpr auto in(TypeArray<Ts...> _ = {}) noexcept {
			return std::integer_sequence<bool, _.contains<Types>()...>{};
		}

		/**
		 * \brief Map to if types is not in Ts.
		 * \tparam Ts
		 * \param _ Ts deduction
		 * \return integer_sequence<bool>{if is not in}
		 */
		template<typename... Ts>
		static constexpr auto not_in(TypeArray<Ts...> _ = {}) noexcept {
			return std::integer_sequence<bool, !_.contains<Types>()...>{};
		}

		/**
		 * \brief Map to nth type of types.\n
		 *		  Note: Defined in "Map.h".
		 * \return the indexes
		 */
		static constexpr auto nth() noexcept;

		/**
		 * \brief Distribute every type in TypeArray<Ts...> to proper indexes of TypeArray<Types...>.
		 *        If can not distribute(TypeArray<Ts...> is not subarray of TypeArray<Types...>), return NullType.\n
		 *		  Note: Defined in "Map.h".
		 * \tparam Ts
		 * \param _ Ts deduction
		 * \return the indexes or NullType
		 */
		template<typename... Ts>
		static constexpr auto distribute(TypeArray<Ts...> _ = {}) noexcept;

		/**
		 * \brief Check if TypeArray<Types...> can distribute TypeArray<Ts...>.\n
		 *        Note: Defined in "Map.h".
		 * \tparam Ts
		 * \param _ Ts deduction
		 * \return if can distribute
		 */
		template<typename... Ts>
		static constexpr bool can_distribute(TypeArray<Ts...> _ = {}) noexcept;

		/**
		 * \brief Map TypeArray<Types...> to TypeArray<F<Types>...>.
		 * \tparam F type map
		 * \return TypeArray<F<Types>...>
		 */
		template<template<typename> typename F>
		static constexpr TypeArray<F<Types>...> map() noexcept { return {}; }
	};
}

#endif
