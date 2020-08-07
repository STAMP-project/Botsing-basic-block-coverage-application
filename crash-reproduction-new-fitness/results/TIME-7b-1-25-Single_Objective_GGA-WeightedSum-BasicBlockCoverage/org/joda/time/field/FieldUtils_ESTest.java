/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 16:49:14 UTC 2020
 */

package org.joda.time.field;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.DateTimeField;
import org.joda.time.DateTimeFieldType;
import org.joda.time.chrono.CopticChronology;
import org.joda.time.field.FieldUtils;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class FieldUtils_ESTest extends FieldUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DateTimeFieldType dateTimeFieldType0 = DateTimeFieldType.weekyearOfCentury();
      CopticChronology copticChronology0 = CopticChronology.getInstance();
      DateTimeField dateTimeField0 = dateTimeFieldType0.getField(copticChronology0);
      int int0 = 2005;
      int int1 = 1217;
      // Undeclared exception!
      FieldUtils.verifyValueBounds(dateTimeField0, 2005, 2005, 1217);
  }
}
