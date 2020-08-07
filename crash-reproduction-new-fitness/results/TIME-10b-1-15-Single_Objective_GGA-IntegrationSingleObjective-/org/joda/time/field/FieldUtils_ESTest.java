/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 00:21:11 UTC 2020
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
import org.joda.time.field.DelegatedDateTimeField;
import org.joda.time.field.FieldUtils;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class FieldUtils_ESTest extends FieldUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DateTimeField dateTimeField0 = mock(DateTimeField.class, new ViolatedAssumptionAnswer());
      FieldUtils.verifyValueBounds(dateTimeField0, Integer.MAX_VALUE, Integer.MAX_VALUE, Integer.MAX_VALUE);
      int int0 = (-338);
      FieldUtils.safeMultiply((long) (-338), 1L);
      FieldUtils.safeSubtract(1L, 107730L);
      int int1 = (-578);
      int int2 = 0;
      int int3 = (-270);
      BuddhistChronology buddhistChronology0 = BuddhistChronology.getInstanceUTC();
      DateTimeField dateTimeField1 = buddhistChronology0.weekyear();
      DelegatedDateTimeField delegatedDateTimeField0 = new DelegatedDateTimeField(dateTimeField1);
      // Undeclared exception!
      FieldUtils.verifyValueBounds((DateTimeField) delegatedDateTimeField0, (-270), (-3034), (-3034));
  }
}
