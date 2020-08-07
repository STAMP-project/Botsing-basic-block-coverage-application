/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Sat May 16 12:16:40 UTC 2020
 */

package org.apache.commons.math3.geometry.euclidean.twod;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

@EvoSuiteClassExclude
public class PolygonsSet_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.apache.commons.math3.geometry.euclidean.twod.PolygonsSet"; 
    org.evosuite.runtime.GuiSupport.initialize(); 
    org.evosuite.runtime.RuntimeSettings.maxNumberOfIterationsPerLoop = 10000; 
    org.evosuite.runtime.RuntimeSettings.mockSystemIn = true; 
    org.evosuite.runtime.Runtime.getInstance().resetRuntime(); 
  } 

  @Before 
  public void initTestCase(){ 
    threadStopper.storeCurrentThreads();
    threadStopper.startRecordingTime();
    org.evosuite.runtime.GuiSupport.setHeadless(); 
    org.evosuite.runtime.Runtime.getInstance().resetRuntime(); 
    org.evosuite.runtime.agent.InstrumentingAgent.activate(); 
  } 

  @After 
  public void doneWithTestCase(){ 
    threadStopper.killAndJoinClientThreads();
    org.evosuite.runtime.agent.InstrumentingAgent.deactivate(); 
    org.evosuite.runtime.GuiSupport.restoreHeadlessMode(); 
  } 


  private static void initializeClasses() {
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(PolygonsSet_ESTest_scaffolding.class.getClassLoader() ,
      "org.apache.commons.math3.geometry.euclidean.threed.PolyhedronsSet$TranslationTransform",
      "org.apache.commons.math3.geometry.Space",
      "org.apache.commons.math3.geometry.euclidean.threed.NotARotationMatrixException",
      "org.apache.commons.math3.geometry.partitioning.RegionFactory",
      "org.apache.commons.math3.geometry.euclidean.twod.Segment",
      "org.apache.commons.math3.geometry.euclidean.oned.Vector1D",
      "org.apache.commons.math3.geometry.partitioning.utilities.AVLTree$1",
      "org.apache.commons.math3.geometry.euclidean.oned.SubOrientedPoint",
      "org.apache.commons.math3.geometry.partitioning.RegionFactory$UnionMerger",
      "org.apache.commons.math3.geometry.euclidean.threed.OutlineExtractor$BoundaryProjector",
      "org.apache.commons.math3.exception.util.ArgUtils",
      "org.apache.commons.math3.exception.MathArithmeticException",
      "org.apache.commons.math3.geometry.euclidean.twod.PolygonsSet$ComparableSegment",
      "org.apache.commons.math3.geometry.partitioning.BSPTreeVisitor",
      "org.apache.commons.math3.exception.MathInternalError",
      "org.apache.commons.math3.exception.NonMonotonicSequenceException",
      "org.apache.commons.math3.geometry.partitioning.AbstractRegion$3",
      "org.apache.commons.math3.geometry.euclidean.oned.Euclidean1D$1",
      "org.apache.commons.math3.exception.MathIllegalArgumentException",
      "org.apache.commons.math3.exception.MathUnsupportedOperationException",
      "org.apache.commons.math3.geometry.partitioning.BSPTreeVisitor$Order",
      "org.apache.commons.math3.geometry.euclidean.threed.Line",
      "org.apache.commons.math3.geometry.euclidean.oned.Euclidean1D$LazyHolder",
      "org.apache.commons.math3.geometry.euclidean.threed.Plane",
      "org.apache.commons.math3.geometry.partitioning.AbstractSubHyperplane",
      "org.apache.commons.math3.util.FastMath",
      "org.apache.commons.math3.geometry.partitioning.Transform",
      "org.apache.commons.math3.geometry.euclidean.oned.OrientedPoint",
      "org.apache.commons.math3.geometry.partitioning.BSPTree",
      "org.apache.commons.math3.geometry.partitioning.Characterization",
      "org.apache.commons.math3.geometry.euclidean.twod.Euclidean2D$LazyHolder",
      "org.apache.commons.math3.geometry.euclidean.twod.Line",
      "org.apache.commons.math3.geometry.euclidean.twod.Line$LineTransform",
      "org.apache.commons.math3.geometry.partitioning.RegionFactory$XorMerger",
      "org.apache.commons.math3.exception.NotFiniteNumberException",
      "org.apache.commons.math3.geometry.partitioning.RegionFactory$NodesCleaner",
      "org.apache.commons.math3.geometry.partitioning.RegionFactory$DifferenceMerger",
      "org.apache.commons.math3.geometry.Vector",
      "org.apache.commons.math3.geometry.partitioning.BSPTree$1",
      "org.apache.commons.math3.exception.util.ExceptionContextProvider",
      "org.apache.commons.math3.geometry.partitioning.Side",
      "org.apache.commons.math3.geometry.euclidean.oned.Interval",
      "org.apache.commons.math3.geometry.partitioning.utilities.AVLTree$Skew",
      "org.apache.commons.math3.geometry.euclidean.oned.IntervalsSet",
      "org.apache.commons.math3.util.MathArrays",
      "org.apache.commons.math3.geometry.partitioning.RegionFactory$IntersectionMerger",
      "org.apache.commons.math3.geometry.partitioning.Hyperplane",
      "org.apache.commons.math3.geometry.euclidean.twod.PolygonsSet",
      "org.apache.commons.math3.geometry.partitioning.utilities.AVLTree",
      "org.apache.commons.math3.geometry.euclidean.threed.CardanEulerSingularityException",
      "org.apache.commons.math3.geometry.euclidean.twod.Vector2D",
      "org.apache.commons.math3.geometry.euclidean.threed.SubPlane",
      "org.apache.commons.math3.geometry.euclidean.twod.SubLine",
      "org.apache.commons.math3.geometry.partitioning.BoundaryAttribute",
      "org.apache.commons.math3.geometry.euclidean.threed.PolyhedronsSet$FacetsContributionVisitor",
      "org.apache.commons.math3.exception.MathIllegalStateException",
      "org.apache.commons.math3.geometry.partitioning.SubHyperplane$SplitSubHyperplane",
      "org.apache.commons.math3.geometry.euclidean.twod.Euclidean2D",
      "org.apache.commons.math3.geometry.partitioning.BSPTree$LeafMerger",
      "org.apache.commons.math3.geometry.partitioning.utilities.AVLTree$Node",
      "org.apache.commons.math3.geometry.euclidean.twod.Euclidean2D$1",
      "org.apache.commons.math3.geometry.euclidean.threed.SubLine",
      "org.apache.commons.math3.geometry.partitioning.AbstractRegion$Sides",
      "org.apache.commons.math3.util.MathUtils",
      "org.apache.commons.math3.geometry.partitioning.utilities.OrderedTuple",
      "org.apache.commons.math3.exception.MathIllegalNumberException",
      "org.apache.commons.math3.exception.util.LocalizedFormats",
      "org.apache.commons.math3.geometry.partitioning.AbstractRegion",
      "org.apache.commons.math3.geometry.partitioning.Region",
      "org.apache.commons.math3.geometry.euclidean.threed.PolyhedronsSet$RotationTransform",
      "org.apache.commons.math3.geometry.partitioning.Region$Location",
      "org.apache.commons.math3.exception.DimensionMismatchException",
      "org.apache.commons.math3.geometry.euclidean.oned.Euclidean1D",
      "org.apache.commons.math3.geometry.euclidean.twod.PolygonsSet$SegmentsBuilder",
      "org.apache.commons.math3.geometry.euclidean.threed.PolyhedronsSet",
      "org.apache.commons.math3.exception.util.Localizable",
      "org.apache.commons.math3.geometry.partitioning.BoundarySizeVisitor",
      "org.apache.commons.math3.geometry.euclidean.threed.Rotation",
      "org.apache.commons.math3.exception.util.ExceptionContext",
      "org.apache.commons.math3.geometry.euclidean.threed.Vector3D",
      "org.apache.commons.math3.exception.NullArgumentException",
      "org.apache.commons.math3.geometry.partitioning.Embedding",
      "org.apache.commons.math3.geometry.euclidean.threed.Euclidean3D",
      "org.apache.commons.math3.geometry.euclidean.twod.Line$1",
      "org.apache.commons.math3.geometry.partitioning.SubHyperplane",
      "org.apache.commons.math3.geometry.euclidean.threed.RotationOrder"
    );
  } 
}
