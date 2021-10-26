/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 22:48:38 UTC 2021
 */

package org.xwiki.rendering.internal.macro.toc;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.ArrayList;
import javatests.DiamondIterableMapMRO;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.jgroups.util.TimeScheduler3;
import org.junit.runner.RunWith;
import org.xwiki.rendering.block.Block;
import org.xwiki.rendering.block.MacroBlock;
import org.xwiki.rendering.internal.macro.toc.TreeParametersBuilder;
import org.xwiki.rendering.macro.toc.TocMacroParameters;
import org.xwiki.rendering.transformation.MacroTransformationContext;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class TreeParametersBuilder_ESTest extends TreeParametersBuilder_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      ArrayList<Block> arrayList0 = new ArrayList<Block>();
      Block block0 = null;
      DiamondIterableMapMRO diamondIterableMapMRO0 = new DiamondIterableMapMRO();
      TocMacroParameters tocMacroParameters0 = new TocMacroParameters();
      TocMacroParameters tocMacroParameters1 = new TocMacroParameters();
      TocMacroParameters.Scope tocMacroParameters_Scope0 = TocMacroParameters.Scope.LOCAL;
      tocMacroParameters1.setScope(tocMacroParameters_Scope0);
      MacroTransformationContext macroTransformationContext0 = new MacroTransformationContext();
      MacroTransformationContext macroTransformationContext1 = macroTransformationContext0.clone();
      TreeParametersBuilder treeParametersBuilder0 = new TreeParametersBuilder();
      Object object0 = new Object();
      MacroBlock macroBlock0 = new MacroBlock("#-", diamondIterableMapMRO0, false);
      macroTransformationContext1.setCurrentMacroBlock(macroBlock0);
      TimeScheduler3.Task timeScheduler3_Task0 = new TimeScheduler3.Task(diamondIterableMapMRO0, false);
      treeParametersBuilder0.build((Block) null, tocMacroParameters0, macroTransformationContext1);
      // Undeclared exception!
      treeParametersBuilder0.build((Block) null, tocMacroParameters1, macroTransformationContext1);
  }
}
