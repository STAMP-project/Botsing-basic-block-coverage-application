/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 00:20:45 UTC 2020
 */

package org.joda.time.field;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.DateTimeField;
import org.joda.time.field.FieldUtils;
import org.joda.time.field.TestBaseDateTimeField;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class FieldUtils_ESTest extends FieldUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      TestBaseDateTimeField.MockBaseDateTimeField testBaseDateTimeField_MockBaseDateTimeField0 = new TestBaseDateTimeField.MockBaseDateTimeField();
      int int0 = (-2651);
      int int1 = 111;
      FieldUtils.verifyValueBounds((DateTimeField) testBaseDateTimeField_MockBaseDateTimeField0, (-2651), (-2651), 111);
      int int2 = 0;
      // Undeclared exception!
      FieldUtils.verifyValueBounds((DateTimeField) testBaseDateTimeField_MockBaseDateTimeField0, 0, 0, (-2651));
  }
}
