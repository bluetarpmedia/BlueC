int main(void) {
  unsigned short us;
  
  // Test 1: += operator
  us = 100;
  us += 50;
  if (us != 150) return 1;
  
  // Test 2: -= operator
  us = 100;
  us -= 30;
  if (us != 70) return 2;
  
  // Test 3: *= operator
  us = 100;
  us *= 3;
  if (us != 300) return 3;
  
  // Test 4: /= operator
  us = 100;
  us /= 4;
  if (us != 25) return 4;
  
  // Test 5: %= operator
  us = 100;
  us %= 7;
  if (us != 2) return 5;
  
  // Test 6: &= operator (bitwise AND)
  us = 0xFF;
  us &= 0xF0;
  if (us != 0xF0) return 6;
  
  // Test 7: |= operator (bitwise OR)
  us = 0xF0;
  us |= 0x0F;
  if (us != 0xFF) return 7;
  
  // Test 8: ^= operator (bitwise XOR)
  us = 0xFF;
  us ^= 0x0F;
  if (us != 0xF0) return 8;
  
  // Test 9: <<= operator (left shift)
  us = 0x01;
  us <<= 4;
  if (us != 0x10) return 9;
  
  // Test 10: >>= operator (right shift)
  us = 0x100;
  us >>= 4;
  if (us != 0x10) return 10;
  
  // Test 11: += with overflow
  us = 65535;
  us += 1;
  if (us != 0) return 11;
  
  // Test 12: -= with underflow
  us = 0;
  us -= 1;
  if (us != 65535) return 12;
  
  // Test 13: *= with overflow
  us = 1000;
  us *= 100;
  if (us != (100000 & 0xFFFF)) return 13;
  
  // Test 14: <<= with bit loss
  us = 0xFFFF;
  us <<= 8;
  if (us != 0xFF00) return 14;
  
  // Test 15: >>= to zero
  us = 0xFF;
  us >>= 8;
  if (us != 0) return 15;
  
  return 0; // success
}