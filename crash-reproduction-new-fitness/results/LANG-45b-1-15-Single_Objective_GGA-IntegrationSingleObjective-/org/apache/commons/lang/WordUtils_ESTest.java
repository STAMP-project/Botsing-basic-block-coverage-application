/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:35:21 UTC 2020
 */

package org.apache.commons.lang;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.lang.WordUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class WordUtils_ESTest extends WordUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      WordUtils.capitalize(" ");
      WordUtils.wrap(" ", 2231, " ", false);
      WordUtils.initials("CCG");
      WordUtils.wrap(" ", 2231);
      WordUtils wordUtils0 = new WordUtils();
      char[] charArray0 = new char[8];
      charArray0[0] = '_';
      charArray0[1] = '=';
      charArray0[2] = 'Z';
      charArray0[3] = 'z';
      charArray0[4] = 'b';
      charArray0[5] = 'w';
      charArray0[6] = 'Y';
      charArray0[7] = '9';
      WordUtils.initials("CCG", charArray0);
      WordUtils.capitalizeFully("l", charArray0);
      // Undeclared exception!
      WordUtils.abbreviate("java.vm.specification.name", 578, 1, " ");
  }
}
