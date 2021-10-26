/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 15:23:40 UTC 2021
 */

package org.apache.commons.math.stat.clustering;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
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
      Random random0 = mock(Random.class, new ViolatedAssumptionAnswer());
      KMeansPlusPlusClusterer kMeansPlusPlusClusterer0 = new KMeansPlusPlusClusterer(random0);
      Random random1 = new Random();
      KMeansPlusPlusClusterer<EuclideanIntegerPoint> kMeansPlusPlusClusterer1 = new KMeansPlusPlusClusterer<EuclideanIntegerPoint>(random1);
      LinkedList<EuclideanIntegerPoint> linkedList0 = new LinkedList<EuclideanIntegerPoint>();
      int[] intArray0 = new int[15];
      intArray0[0] = (-1);
      EuclideanIntegerPoint euclideanIntegerPoint0 = new EuclideanIntegerPoint(intArray0);
      euclideanIntegerPoint0.toString();
      linkedList0.offerFirst(euclideanIntegerPoint0);
      intArray0[2] = 0;
      intArray0[3] = (-2147483625);
      intArray0[4] = 2893;
      EuclideanIntegerPoint euclideanIntegerPoint1 = new EuclideanIntegerPoint(intArray0);
      euclideanIntegerPoint1.toString();
      EuclideanIntegerPoint euclideanIntegerPoint2 = euclideanIntegerPoint1.centroidOf(linkedList0);
      linkedList0.add(euclideanIntegerPoint1);
      EuclideanIntegerPoint euclideanIntegerPoint3 = new EuclideanIntegerPoint(intArray0);
      euclideanIntegerPoint2.toString();
      euclideanIntegerPoint2.centroidOf(linkedList0);
      linkedList0.add(euclideanIntegerPoint0);
      EuclideanIntegerPoint euclideanIntegerPoint4 = euclideanIntegerPoint1.centroidOf(linkedList0);
      linkedList0.add(euclideanIntegerPoint4);
      int int0 = 0;
      // Undeclared exception!
      kMeansPlusPlusClusterer1.cluster(linkedList0, 0, 0);
  }
}
