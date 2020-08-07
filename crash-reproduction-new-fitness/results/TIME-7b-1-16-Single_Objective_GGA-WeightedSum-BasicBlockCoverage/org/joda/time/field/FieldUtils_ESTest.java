/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 16:46:54 UTC 2020
 */

package org.joda.time.field;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.DateTimeField;
import org.joda.time.chrono.BuddhistChronology;
import org.joda.time.field.FieldUtils;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class FieldUtils_ESTest extends FieldUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      int int0 = (-542);
      int int1 = 2986;
      FieldUtils.safeMultiply((-542), 2986);
      BuddhistChronology buddhistChronology0 = BuddhistChronology.getInstanceUTC();
      DateTimeField dateTimeField0 = buddhistChronology0.yearOfEra();
      FieldUtils.verifyValueBounds(dateTimeField0, 200, (-1618412), 200);
      FieldUtils.safeMultiply(470L, 1296L);
      int int2 = 1610;
      // Undeclared exception!
      FieldUtils.verifyValueBounds(dateTimeField0, 1610, 200, (-1618412));
  }
}
