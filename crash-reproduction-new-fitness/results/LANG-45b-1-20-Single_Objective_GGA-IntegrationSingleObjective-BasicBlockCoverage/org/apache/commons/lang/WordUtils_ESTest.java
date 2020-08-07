/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:54:57 UTC 2020
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
      WordUtils.wrap("", 0, "", false);
      WordUtils.wrap("", 0);
      WordUtils.abbreviate((String) null, 0, 0, (String) null);
      WordUtils.swapCase((String) null);
      WordUtils.abbreviate("", 3, 0, (String) null);
      WordUtils wordUtils0 = new WordUtils();
      char[] charArray0 = new char[2];
      charArray0[0] = ',';
      charArray0[1] = 'L';
      WordUtils.initials((String) null, charArray0);
      WordUtils.capitalize((String) null);
      WordUtils.uncapitalize("java.home", charArray0);
      WordUtils.initials((String) null);
      WordUtils.swapCase("");
      // Undeclared exception!
      WordUtils.abbreviate("P7yrEXEV6%\"", 971, 1571, "");
  }
}
