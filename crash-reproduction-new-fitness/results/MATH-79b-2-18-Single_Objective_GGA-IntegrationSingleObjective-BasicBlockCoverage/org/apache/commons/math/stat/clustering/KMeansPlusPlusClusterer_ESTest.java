/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 17:06:16 UTC 2020
 */

package org.apache.commons.math.stat.clustering;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.Random;
import org.apache.commons.math.stat.clustering.EuclideanIntegerPoint;
import org.apache.commons.math.stat.clustering.KMeansPlusPlusClusterer;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class KMeansPlusPlusClusterer_ESTest extends KMeansPlusPlusClusterer_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      LinkedList<EuclideanIntegerPoint> linkedList0 = new LinkedList<EuclideanIntegerPoint>();
      LinkedList<String> linkedList1 = new LinkedList<String>();
      LinkedList<String> linkedList2 = new LinkedList<String>();
      LinkedList<String> linkedList3 = new LinkedList<String>();
      LinkedList<String> linkedList4 = new LinkedList<String>();
      Comparator<EuclideanIntegerPoint> comparator0 = (Comparator<EuclideanIntegerPoint>) mock(Comparator.class, new ViolatedAssumptionAnswer());
      linkedList0.sort(comparator0);
      int[] intArray0 = new int[3];
      intArray0[0] = Integer.MAX_VALUE;
      intArray0[1] = 973;
      intArray0[2] = (-2090);
      EuclideanIntegerPoint euclideanIntegerPoint0 = new EuclideanIntegerPoint(intArray0);
      linkedList0.add(euclideanIntegerPoint0);
      EuclideanIntegerPoint euclideanIntegerPoint1 = new EuclideanIntegerPoint(intArray0);
      euclideanIntegerPoint0.centroidOf(linkedList0);
      euclideanIntegerPoint0.distanceFrom(euclideanIntegerPoint1);
      linkedList0.add(euclideanIntegerPoint1);
      linkedList0.add(euclideanIntegerPoint1);
      Random random0 = new Random();
      KMeansPlusPlusClusterer<EuclideanIntegerPoint> kMeansPlusPlusClusterer0 = new KMeansPlusPlusClusterer<EuclideanIntegerPoint>(random0);
      // Undeclared exception!
      kMeansPlusPlusClusterer0.cluster(linkedList0, (-1), (-3252));
  }
}
