/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 12:26:22 UTC 2020
 */

package org.apache.commons.math.stat;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Comparator;
import org.apache.commons.math.stat.Frequency;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Frequency_ESTest extends Frequency_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Comparator<Long> comparator0 = (Comparator<Long>) mock(Comparator.class, new ViolatedAssumptionAnswer());
      doReturn(0, 0, 0, 0, 0).when(comparator0).compare(anyLong() , anyLong());
      Frequency frequency0 = new Frequency(comparator0);
      frequency0.toString();
      frequency0.getCumPct('y');
      frequency0.valuesIterator();
      frequency0.getPct(1548);
      frequency0.getSumFreq();
      frequency0.getPct((Object) "Value \t Freq. \t Pct. \t Cum Pct. \n");
      frequency0.addValue('y');
      Integer integer0 = new Integer(1548);
      frequency0.addValue(integer0);
      char char0 = 'v';
      frequency0.getCount('v');
      frequency0.getCumFreq('v');
      Object object0 = new Object();
      // Undeclared exception!
      frequency0.addValue(object0);
  }
}
