cmake_minimum_required(VERSION 3.5)

include(ParseCliArguments)

parse_cli_arguments(
  POSITIONALS
  SOURCE_FILE
  KEYWORDS
  INCLUDE_DIRECTORIES
  INTERFACE_INCLUDE_DIRECTORIES
  COMPILE_DEFINITIONS
  INTERFACE_COMPILE_DEFINITIONS
  COMPILE_FEATURES
  INTERFACE_COMPILE_FEATURES
  COMPILE_OPTIONS
  INTERFACE_COMPILE_OPTIONS
  OPTIONS
  IS_SOURCE)

find_program(IWYU_COMMAND include-what-you-use
             DOC "Check inclusion in C and C++ programs" REQUIRED)

if(NOT IS_SOURCE)
  set(INCLUDE_DIRECTORIES ${INTERFACE_INCLUDE_DIRECTORIES})
  set(COMPILE_DEFINITIONS ${INTERFACE_COMPILE_DEFINITIONS})
  set(COMPILE_FEATURES ${INTERFACE_COMPILE_FEATURES})
  set(COMPILE_OPTIONS ${INTERFACE_COMPILE_OPTIONS})
endif()

cmake_path(CONVERT "${SOURCE_FILE}" TO_NATIVE_PATH_LIST SOURCE_FILE)
cmake_path(CONVERT "${INCLUDE_DIRECTORIES}" TO_NATIVE_PATH_LIST
           INCLUDE_DIRECTORIES)

set(ISYSTEM_FLAGS ${INCLUDE_DIRECTORIES})
list(TRANSFORM ISYSTEM_FLAGS PREPEND "-isystem ")

set(DEFINITION_FLAGS ${COMPILE_DEFINITIONS})
list(TRANSFORM DEFINITION_FLAGS PREPEND "-D ")

set(COMPILER_OPTION_FLAGS ${COMPILE_OPTIONS})

if(cxx_std_17 IN_LIST COMPILE_FEATURES)
  list(APPEND COMPILE_OPTION_FLAGS -std=c++17)
endif()

execute_process(
  COMMAND ${IWYU_COMMAND} ${SOURCE_FILE} ${ISYSTEM_FLAGS} ${DEFINITION_FLAGS}
          ${COMPILE_OPTION_FLAGS} -Xiwyu --error=1 -Xiwyu --no_fwd_decls
  RESULT_VARIABLE COMMAND_RESULT
  OUTPUT_VARIABLE COMMAND_OUTPUT
  ERROR_VARIABLE COMMAND_OUTPUT)

if(NOT COMMAND_RESULT EQUAL 0)
  message(${COMMAND_OUTPUT})
  message(
    FATAL_ERROR
      "Include What You Use failed on ${SOURCE_FILE} with error code ${COMMAND_RESULT}"
  )
endif()