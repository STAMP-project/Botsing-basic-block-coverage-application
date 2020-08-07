/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 13:21:37 UTC 2020
 */

package org.joda.time.field;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.joda.time.DateTimeField;
import org.joda.time.chrono.BuddhistChronology;
import org.joda.time.field.FieldUtils;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class FieldUtils_ESTest extends FieldUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DateTimeField dateTimeField0 = mock(DateTimeField.class, new ViolatedAssumptionAnswer());
      int int0 = (-3226);
      FieldUtils.verifyValueBounds(dateTimeField0, (-3226), (-3226), (-3226));
      int int1 = (-1);
      FieldUtils.safeMultiply((-3226), (-1));
      long long0 = 0L;
      FieldUtils.safeMultiplyToInt(0L, 0L);
      Object object0 = new Object();
      Object object1 = new Object();
      BuddhistChronology buddhistChronology0 = BuddhistChronology.getInstanceUTC();
      DateTimeField dateTimeField1 = buddhistChronology0.hourOfHalfday();
      int int2 = 3226;
      // Undeclared exception!
      FieldUtils.verifyValueBounds(dateTimeField1, 3226, 3570, (-2162));
  }
}
