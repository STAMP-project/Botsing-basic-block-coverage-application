/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 20:58:15 UTC 2020
 */

package org.apache.commons.lang;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.LinkedList;
import org.apache.commons.lang.LocaleUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class LocaleUtils_ESTest extends LocaleUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      LinkedList<LinkedList<String>> linkedList0 = new LinkedList<LinkedList<String>>();
      LinkedList<String> linkedList1 = new LinkedList<String>();
      // Undeclared exception!
      LocaleUtils.toLocale("lk_<`r0");
  }
}
