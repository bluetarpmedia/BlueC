int main(void) {
  unsigned short us;
  unsigned int ui;
  int si;
  
  // Test 1: Truncate unsigned int that fits
  ui = 1000;
  us = (unsigned short)ui;
  if (us != 1000) return 1;
  
  // Test 2: Truncate unsigned int larger than USHRT_MAX
  ui = 65536;  // 0x10000
  us = (unsigned short)ui;
  if (us != 0) return 2;
  
  // Test 3: Truncate unsigned int = USHRT_MAX + 1
  ui = 65535 + 1;
  us = (unsigned short)ui;
  if (us != 0) return 3;
  
  // Test 4: Truncate unsigned int = USHRT_MAX + 10
  ui = 65535 + 10;
  us = (unsigned short)ui;
  if (us != 9) return 4;
  
  // Test 5: Truncate large unsigned int (keeps lower 16 bits)
  ui = 0x12345;  // 74565
  us = (unsigned short)ui;
  if (us != 0x2345) return 5;  // 9029
  
  // Test 6: Truncate positive signed int that fits
  si = 5000;
  us = (unsigned short)si;
  if (us != 5000) return 6;
  
  // Test 7: Truncate negative signed int
  si = -1;
  us = (unsigned short)si;
  if (us != 65535) return 7;
  
  // Test 8: Truncate negative signed int (-100)
  si = -100;
  us = (unsigned short)si;
  if (us != 65535 - 99) return 8;
  
  // Test 9: Truncate signed int larger than USHRT_MAX
  si = 70000;
  us = (unsigned short)si;
  if (us != (70000 & 0xFFFF)) return 9;
  
  // Test 10: Truncate zero
  ui = 0;
  us = (unsigned short)ui;
  if (us != 0) return 10;
  
  // Test 11: Truncate USHRT_MAX exactly
  ui = 65535;
  us = (unsigned short)ui;
  if (us != 65535) return 11;
  
  // Test 12: Truncate with bitwise pattern (alternate bits)
  ui = 0xAAAAAAAA;
  us = (unsigned short)ui;
  if (us != 0xAAAA) return 12;
  
  return 0; // success
}