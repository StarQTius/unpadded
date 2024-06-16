#pragma once

#include <cstddef>
#include <cstdint>
#include <functional>
#include <limits>
#include <tuple>
#include <type_traits>

#include "detail/integral_constant.hpp"
#include "upd.hpp"
#include "upd/description.hpp"

namespace upd {

template<typename... Ts>
class description {
  template<typename... Ts_, typename... Us>
  friend constexpr auto operator|(description<Ts_...> lhs, description<Us...> rhs) noexcept;

public:
  explicit constexpr description(Ts... xs) : m_fields{UPD_FWD(xs)...} {}

  [[nodiscard]] constexpr auto fields() const & -> std::tuple<Ts...> { return m_fields; }

  [[nodiscard]] constexpr auto fields() && -> std::tuple<Ts...> { return std::move(m_fields); }

private:
  std::tuple<Ts...> m_fields;
};

template<typename... Ts, typename... Us>
constexpr inline auto operator|(description<Ts...> lhs, description<Us...> rhs) noexcept {
  auto fields = std::tuple_cat(std::move(lhs.m_fields), std::move(rhs.m_fields));
  return std::make_from_tuple<description<Ts..., Us...>>(std::move(fields));
}

template<bool Is_Signed>
struct signedness_t {};

constexpr inline auto signed_int = signedness_t<true>{};
constexpr inline auto unsigned_int = signedness_t<false>{};

template<std::size_t Width>
struct width_t {};

template<std::size_t Width>
constexpr auto width = width_t<Width>{};

template<typename Id, typename Is_Signed, typename Width>
struct field_t {
  constexpr static auto id = Id::value;
  constexpr static auto is_signed = Is_Signed::value;
  constexpr static auto width = Width::value;
};

template<auto Id, bool Is_Signed, std::size_t Width>
[[nodiscard]] constexpr auto field(signedness_t<Is_Signed>, width_t<Width>) noexcept {
  using id = detail::integral_constant_t<Id>;
  using is_signed = detail::integral_constant_t<Is_Signed>;
  using width = detail::integral_constant_t<Width>;

  auto retval = field_t<id, is_signed, width>{};
  return description{retval};
}

template<auto Id, bool Is_Signed, std::size_t Width, typename Get_Value_F>
class constrained_field_t {
public:
  constexpr explicit constrained_field_t(Get_Value_F get_value) : m_get_value{UPD_FWD(get_value)} {}

  constexpr static auto id = Id;
  constexpr static auto is_signed = Is_Signed;
  constexpr static auto width = Width;

private:
  Get_Value_F m_get_value;
};

template<auto Id, bool Is_Signed, std::size_t Width, typename Get_Value_F>
[[nodiscard]] constexpr auto
constrained_field(signedness_t<Is_Signed>, width_t<Width>, Get_Value_F &&get_value) noexcept {
  auto retval = constrained_field_t<Id, Is_Signed, Width, Get_Value_F>{UPD_FWD(get_value)};
  return description{retval};
}

template<auto Id, typename Decorator_F, typename... Ts>
class group_t {
public:
  explicit constexpr group_t(Decorator_F decorator, description<Ts...> descr) noexcept
      : m_decorator{UPD_FWD(decorator)}, m_descr(std::move(descr)) {}

  template<typename It, typename... Us>
  [[nodiscard]] constexpr auto decorate(It &it, const description<Us...> &parent_descr) noexcept {
    return std::invoke(m_decorator, it, parent_descr);
  }

private:
  Decorator_F m_decorator;
  description<Ts...> m_descr;
};

template<auto Id, typename Decorator_F, typename... Ts>
[[nodiscard]] constexpr auto group(Decorator_F &&decorator, description<Ts...> descr) {
  auto retval = group_t<Id, std::decay_t<Decorator_F>, Ts...>{std::move(descr)};

  return description{UPD_FWD(decorator), std::move(retval)};
}

} // namespace upd

namespace upd::literals {

[[nodiscard]] constexpr inline auto operator""_h(const char *str, std::size_t size) noexcept -> std::size_t {
  constexpr auto numlim = std::numeric_limits<char>{};
  constexpr auto min = std::intmax_t{numlim.min()};
  constexpr auto max = std::intmax_t{numlim.max()};

  auto retval = std::size_t{0};

  // `std::hash` cannot be invoked in constant expression, so here is the poor man's hashing function for the moment
  for (std::size_t i = 0; i < size; ++i) {
    retval += str[i] - min;
    retval *= max - min;
  }

  return retval;
}

} // namespace upd::literals