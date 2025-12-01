int main(void) {
  unsigned short us;
  unsigned short us2;
  unsigned short result;
  
  // Test 1: & operator (bitwise AND)
  us = 0xFF;
  us2 = 0xF0;
  result = us & us2;
  if (result != 0xF0) return 1;
  
  // Test 2: | operator (bitwise OR)
  us = 0xF0;
  us2 = 0x0F;
  result = us | us2;
  if (result != 0xFF) return 2;
  
  // Test 3: ^ operator (bitwise XOR)
  us = 0xFF;
  us2 = 0x0F;
  result = us ^ us2;
  if (result != 0xF0) return 3;
  
  // Test 4: ~ operator (bitwise NOT)
  us = 0x00FF;
  result = ~us;
  if (result != 0xFF00) return 4;
  
  // Test 5: << operator (left shift)
  us = 0x01;
  result = us << 4;
  if (result != 0x10) return 5;
  
  // Test 6: >> operator (right shift)
  us = 0x100;
  result = us >> 4;
  if (result != 0x10) return 6;
  
  // Test 7: & with all bits set
  us = 0xFFFF;
  us2 = 0xFFFF;
  result = us & us2;
  if (result != 0xFFFF) return 7;
  
  // Test 8: | with zero
  us = 0x1234;
  us2 = 0x0000;
  result = us | us2;
  if (result != 0x1234) return 8;
  
  // Test 9: ^ with itself (should be zero)
  us = 0xABCD;
  result = us ^ us;
  if (result != 0) return 9;
  
  // Test 10: ~ of zero
  us = 0;
  result = ~us;
  if (result != 0xFFFF) return 10;
  
  // Test 11: << by 0 (no change)
  us = 0x1234;
  result = us << 0;
  if (result != 0x1234) return 11;
  
  // Test 12: >> by 0 (no change)
  us = 0x1234;
  result = us >> 0;
  if (result != 0x1234) return 12;
  
  // Test 13: << with overflow (bits lost)
  us = 0xFFFF;
  result = us << 8;
  if (result != 0xFF00) return 13;
  
  // Test 14: >> to zero
  us = 0x00FF;
  result = us >> 8;
  if (result != 0) return 14;
  
  // Test 15: & with alternating bits
  us = 0xAAAA;
  us2 = 0x5555;
  result = us & us2;
  if (result != 0) return 15;
  
  // Test 16: | with alternating bits
  us = 0xAAAA;
  us2 = 0x5555;
  result = us | us2;
  if (result != 0xFFFF) return 16;
  
  // Test 17: ^ with all ones (inverts)
  us = 0x00FF;
  us2 = 0xFFFF;
  result = us ^ us2;
  if (result != 0xFF00) return 17;
  
  // Test 18: << maximum shift (15 bits)
  us = 0x0001;
  result = us << 15;
  if (result != 0x8000) return 18;
  
  // Test 19: >> maximum shift (15 bits)
  us = 0x8000;
  result = us >> 15;
  if (result != 0x0001) return 19;
  
  // Test 20: Complex expression with multiple operators
  us = 0x00FF;
  us2 = 0xFF00;
  result = (us & 0x0F0F) | (us2 >> 8);
  if (result != 0x00FF) return 20;
  
  return 0; // success
}