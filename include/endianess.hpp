#pragma once

#include "type.hpp"

/*!
  \file
  \brief Definitions for deducing platform specificity
*/

namespace upd {

/*!
  \brief Used to specify endianess
*/
enum class endianess { LITTLE, BIG };

/*!
  \brief Used to specify signed integer representation
*/
enum class signed_mode { SIGNED_MAGNITUDE, ONE_COMPLEMENT, TWO_COMPLEMENT, OFFSET_BINARY, NEGATIVE_BASE };

} // namespace upd
