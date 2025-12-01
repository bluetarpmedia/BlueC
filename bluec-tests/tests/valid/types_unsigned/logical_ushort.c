int main(void) {
  unsigned short us;
  unsigned short us2;
  int result;
  
  // Test 1: ! operator on non-zero value
  us = 100;
  result = !us;
  if (result != 0) return 1;
  
  // Test 2: ! operator on zero
  us = 0;
  result = !us;
  if (result != 1) return 2;
  
  // Test 3: && operator (both true)
  us = 100;
  us2 = 200;
  result = us && us2;
  if (result != 1) return 3;
  
  // Test 4: && operator (first false)
  us = 0;
  us2 = 100;
  result = us && us2;
  if (result != 0) return 4;
  
  // Test 5: && operator (second false)
  us = 100;
  us2 = 0;
  result = us && us2;
  if (result != 0) return 5;
  
  // Test 6: && operator (both false)
  us = 0;
  us2 = 0;
  result = us && us2;
  if (result != 0) return 6;
  
  // Test 7: || operator (both true)
  us = 100;
  us2 = 200;
  result = us || us2;
  if (result != 1) return 7;
  
  // Test 8: || operator (first true)
  us = 100;
  us2 = 0;
  result = us || us2;
  if (result != 1) return 8;
  
  // Test 9: || operator (second true)
  us = 0;
  us2 = 100;
  result = us || us2;
  if (result != 1) return 9;
  
  // Test 10: || operator (both false)
  us = 0;
  us2 = 0;
  result = us || us2;
  if (result != 0) return 10;
  
  // Test 11: ! on USHRT_MAX
  us = 65535;
  result = !us;
  if (result != 0) return 11;
  
  // Test 12: ! on 1
  us = 1;
  result = !us;
  if (result != 0) return 12;
  
  // Test 13: Double negation (!!) on non-zero
  us = 42;
  result = !!us;
  if (result != 1) return 13;
  
  // Test 14: Double negation (!!) on zero
  us = 0;
  result = !!us;
  if (result != 0) return 14;
  
  // Test 15: Complex && expression
  us = 10;
  us2 = 20;
  result = (us > 5) && (us2 < 30);
  if (result != 1) return 15;
  
  // Test 16: Complex || expression
  us = 10;
  us2 = 20;
  result = (us < 5) || (us2 > 15);
  if (result != 1) return 16;
  
  // Test 17: Mixed && and ||
  us = 10;
  us2 = 20;
  result = (us > 0) && (us2 > 0) || (us == 0);
  if (result != 1) return 17;
  
  // Test 18: Logical operators with comparisons
  us = 100;
  us2 = 200;
  result = (us < us2) && (us != 0);
  if (result != 1) return 18;
  
  // Test 19: Short-circuit evaluation (&&)
  us = 0;
  us2 = 100;
  result = us && (us2 / us);  // Should not divide by zero
  if (result != 0) return 19;
  
  // Test 20: Negation in condition
  us = 0;
  result = !us ? 1 : 0;
  if (result != 1) return 20;
  
  return 0; // success
}