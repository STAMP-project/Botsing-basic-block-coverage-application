/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:35:39 UTC 2020
 */

package org.apache.commons.lang;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.lang.BooleanUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BooleanUtils_ESTest extends BooleanUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Boolean[] booleanArray0 = new Boolean[0];
      Boolean.valueOf("The array must not contain any null elements");
      Boolean.valueOf(true);
      BooleanUtils.toStringTrueFalse(false);
      BooleanUtils booleanUtils0 = new BooleanUtils();
      BooleanUtils.toInteger(false);
      // Undeclared exception!
      BooleanUtils.toBoolean("tru");
  }
}
