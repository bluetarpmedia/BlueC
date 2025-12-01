int main(void) {
  unsigned short us;
  
  // Test 1
  us = 0;
  if (us != 0) return 1;
  
  // Test 2
  us = 65535;
  if (us < 65535) return 2;
  
  // Test 3: Test overflow behavior (wraps around to 0)
  us = 65535;
  us = us + 1;
  if (us != 0) return 3;
  
  // Test 4: Test underflow behavior (wraps around to max)
  us = 0;
  us = us - 1;
  if (us != 65535) return 4;
  
  // Test 5: Test arithmetic operations
  us = 100;
  us = us + 50;
  if (us != 150) return 5;

  us = 100;
  us = us - 50;
  if (us != 50) return 5;
  
  // Test 6: Test multiplication
  us = 100;
  us = us * 2;
  if (us != 200) return 6;
  
  // Test 7: Test division
  us = 100;
  us = us / 4;
  if (us != 25) return 7;
  
  // Test 8: Test modulo operation
  us = 100;
  us = us % 7;
  if (us != 2) return 8;
  
  // Test 9: Test bitwise operations
  us = 0xFF;  // 255
  us = us << 4;  // Should be 4080
  if (us != 4080) return 9;
  
  // Test 10: Test bitwise AND
  us = 0xF0F0 & 0xFF00;
  if (us != 0xF000) return 10;
  
  // Test 11: Test comparison with negative (should always be >= 0)
  us = 0;
  if (us < 0) return 11;

  // Test 12: incr/decr
  us = 100;
  unsigned short old = us++;
  if (us != 101) return 12;
  if (old != 100) return 13;
  --us;
  if (us != 100) return 14;
  
  return 0; // success
}
