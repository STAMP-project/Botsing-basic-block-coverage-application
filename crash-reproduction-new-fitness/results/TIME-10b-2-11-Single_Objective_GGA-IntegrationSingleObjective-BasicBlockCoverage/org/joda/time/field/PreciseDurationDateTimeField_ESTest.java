/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 00:21:11 UTC 2020
 */

package org.joda.time.field;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.DateTimeFieldType;
import org.joda.time.LocalDateTime;
import org.joda.time.field.TestPreciseDateTimeField;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class PreciseDurationDateTimeField_ESTest extends PreciseDurationDateTimeField_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DateTimeFieldType.dayOfWeek();
      TestPreciseDateTimeField.MockPreciseDateTimeField testPreciseDateTimeField_MockPreciseDateTimeField0 = new TestPreciseDateTimeField.MockPreciseDateTimeField();
      LocalDateTime localDateTime0 = LocalDateTime.now();
      testPreciseDateTimeField_MockPreciseDateTimeField0.roundHalfCeiling(604800000L);
      int int0 = 10;
      int int1 = 364;
      // Undeclared exception!
      localDateTime0.withDate(10, 10, 364);
  }
}
