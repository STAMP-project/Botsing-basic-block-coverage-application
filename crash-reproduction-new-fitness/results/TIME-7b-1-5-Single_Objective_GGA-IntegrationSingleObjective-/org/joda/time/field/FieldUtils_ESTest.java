/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 13:19:53 UTC 2020
 */

package org.joda.time.field;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.DateTimeField;
import org.joda.time.DateTimeZone;
import org.joda.time.chrono.BuddhistChronology;
import org.joda.time.field.DelegatedDateTimeField;
import org.joda.time.field.FieldUtils;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class FieldUtils_ESTest extends FieldUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BuddhistChronology buddhistChronology0 = BuddhistChronology.getInstance((DateTimeZone) null);
      DateTimeField dateTimeField0 = buddhistChronology0.secondOfDay();
      DelegatedDateTimeField delegatedDateTimeField0 = new DelegatedDateTimeField(dateTimeField0);
      // Undeclared exception!
      FieldUtils.verifyValueBounds((DateTimeField) delegatedDateTimeField0, 44, 0, (-1928));
  }
}
