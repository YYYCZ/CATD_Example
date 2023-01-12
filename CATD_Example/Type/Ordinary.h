/*****************************************************************//**
 * \file   Ordinary.h
 * \brief  Some ordinary type utils.
 *
 * \author YYYCZ
 * \date   December 2022
 *********************************************************************/

#ifndef __YTYPE_ORDINARY_H__
#define __YTYPE_ORDINARY_H__

#include <type_traits>

namespace ytype {
	/**
	 * \brief Null type.
	 */
	struct NullType {};

	/**
	 * \brief Judge if T is NullType.
	 * \tparam T
	 * \return if T is NullType
	 */
	template<typename T>
	static constexpr bool isNullType = std::is_same_v<T, NullType>;

	/**
	 * \brief Type wrapper.
	 * \tparam T
	 */
	template<typename T>
	struct Wrapper {
		using type = T;
		Wrapper() = default;
	};

	/**
	 * \brief Assitant type tp get the type in wrapper.
	 * \tparam T
	 */
	template<typename T>
	struct _Get_wrap_type { using type = T; };

	template<typename T>
	struct _Get_wrap_type<Wrapper<T>> { using type = T; };

	/**
	 * \brief Get the T in wrapper (if in wrapper).
	 * \tparam T
	 * \return the type be wrapped
	 */
	template<typename T>
	using getWrapType = typename _Get_wrap_type<T>::type;

	/**
	 * \brief Judge if T is Wrapper.
	 * \tparam T
	 * \return if T is Wrapper
	 */
	template<typename T>
	static constexpr bool isWrapper = !std::is_same_v<getWrapType<T>, T>;
}

#endif
