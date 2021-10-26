/*
 * This file was automatically generated by EvoSuite
 * Tue Oct 26 06:36:05 UTC 2021
 */

package org.joda.time.format;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Locale;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.joda.time.Chronology;
import org.joda.time.DateTimeFieldType;
import org.joda.time.DateTimeZone;
import org.joda.time.format.DateTimeParserBucket;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class DateTimeParserBucket_ESTest extends DateTimeParserBucket_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Chronology chronology0 = mock(Chronology.class, new ViolatedAssumptionAnswer());
      doReturn((DateTimeZone) null).when(chronology0).getZone();
      doReturn((Chronology) null).when(chronology0).withUTC();
      DateTimeParserBucket dateTimeParserBucket0 = new DateTimeParserBucket((-413L), chronology0, (Locale) null);
      DateTimeFieldType dateTimeFieldType0 = DateTimeFieldType.weekOfWeekyear();
      Locale locale0 = Locale.SIMPLIFIED_CHINESE;
      Locale locale1 = locale0.stripExtensions();
      dateTimeParserBucket0.saveField(dateTimeFieldType0, (String) null, locale1);
      // Undeclared exception!
      dateTimeParserBucket0.computeMillis(false, (String) null);
  }
}
