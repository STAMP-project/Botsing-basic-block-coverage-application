/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 15:59:11 UTC 2020
 */

package org.apache.commons.lang3.time;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.text.ParsePosition;
import java.util.Locale;
import java.util.SimpleTimeZone;
import java.util.TimeZone;
import org.apache.commons.lang3.time.FastDateParser;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class FastDateParser_ESTest extends FastDateParser_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      TimeZone timeZone0 = mock(TimeZone.class, new ViolatedAssumptionAnswer());
      int int0 = (-1364);
      ParsePosition parsePosition0 = new ParsePosition((-2096));
      SimpleTimeZone simpleTimeZone0 = new SimpleTimeZone((-1364), "GMT");
      String string0 = "^>\"@;SWrx`+=ay')o[k";
      SimpleTimeZone simpleTimeZone1 = new SimpleTimeZone((-1364), "^>\"@;SWrx`+=ay')o[k");
      TimeZone timeZone1 = TimeZone.getDefault();
      Locale locale0 = FastDateParser.JAPANESE_IMPERIAL;
      FastDateParser fastDateParser0 = new FastDateParser("GMT", timeZone1, locale0);
  }
}
