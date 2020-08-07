/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 03:29:15 UTC 2020
 */

package org.apache.commons.math.stat;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Comparator;
import java.util.Iterator;
import org.apache.commons.math.stat.Frequency;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Frequency_ESTest extends Frequency_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Comparator<String> comparator0 = (Comparator<String>) mock(Comparator.class, new ViolatedAssumptionAnswer());
      doReturn(0, 0, 0, 0, 0).when(comparator0).compare(anyString() , anyString());
      Frequency frequency0 = new Frequency(comparator0);
      Frequency frequency1 = new Frequency();
      Integer integer0 = new Integer(0);
      frequency0.getCount((Object) integer0);
      frequency0.toString();
      frequency1.addValue(0);
      Iterator iterator0 = frequency1.valuesIterator();
      frequency0.addValue(0);
      Integer integer1 = new Integer(0);
      frequency0.addValue((Object) integer1);
      frequency0.getCumPct((Object) iterator0);
      frequency1.getPct(1216);
      frequency0.getCumFreq((-3940));
      // Undeclared exception!
      frequency1.addValue((Object) iterator0);
  }
}
