/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 12:26:44 UTC 2020
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
      Comparator<Integer> comparator0 = (Comparator<Integer>) mock(Comparator.class, new ViolatedAssumptionAnswer());
      doReturn(0, 0, 0).when(comparator0).compare(anyInt() , anyInt());
      Frequency frequency0 = new Frequency(comparator0);
      frequency0.getPct(0L);
      String string0 = "\\0qh(ZD,R-bJ\"F0udg|";
      frequency0.getPct((Object) "\u0000qh(ZD,R-bJ\"F0udg|");
      frequency0.getCumPct('A');
      frequency0.getCumFreq(0L);
      frequency0.addValue((Object) "\u0000qh(ZD,R-bJ\"F0udg|");
      frequency0.getSumFreq();
      frequency0.addValue((Object) "\u0000qh(ZD,R-bJ\"F0udg|");
      Iterator iterator0 = frequency0.valuesIterator();
      // Undeclared exception!
      frequency0.addValue((Object) iterator0);
  }
}
