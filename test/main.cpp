#include "main.hpp"

int main() {
  UNITY_BEGIN();

  RUN_TEST(order_DO_serialize_argument_into_stream_EXPECT_order_getting_unaltered_argument);
  RUN_TEST(order_DO_give_then_return_argument_from_order_EXPECT_unaltered_value);
  RUN_TEST(order_DO_instantiate_order_with_functor_taking_arguments_EXPECT_input_and_output_sizes_correct);
  RUN_TEST(order_DO_instantiate_order_with_functor_taking_no_arguments_EXPECT_input_and_output_sizes_correct);
  RUN_TEST(order_DO_instantiate_order_with_functor_returning_non_tuple_EXPECT_input_and_output_sizes_correct);
  RUN_TEST(order_DO_instantiate_order_with_functor_non_returning_EXPECT_input_and_output_sizes_correct);

  RUN_TEST(key_base_DO_serialize_arguments_EXPECT_correct_byte_sequence);
  RUN_TEST(key_base_DO_serialize_arguments_with_parameter_EXPECT_correct_byte_sequence);
  RUN_TEST(key_base_DO_unserialize_data_sequence_EXPECT_correct_value);
  RUN_TEST(key_base_DO_unserialize_data_sequence_with_parameter_EXPECT_correct_value);
  RUN_TEST(key_base_DO_create_key_from_ftor_signature_EXPECT_key_holding_ftor_signature);
  RUN_TEST(key_base_DO_create_key_from_function_EXPECT_key_holding_function_signature_cpp17);

  RUN_TEST(key_DO_serialize_argument_EXPECT_correct_id_and_result);

  RUN_TEST(keyring_DO_get_an_ikey_EXPECT_correct_index_cpp17);
  RUN_TEST(keyring_DO_get_an_ikey_by_variable_EXPECT_correct_index_cpp17);

  RUN_TEST(keyring11_DO_get_an_ikey_EXPECT_correct_index);

  RUN_TEST(dispatcher_DO_call_order_EXPECT_calling_correct_order);

  return UNITY_END();
}
