/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 06:09:17 UTC 2020
 */

package org.xwiki.rendering.internal.macro.toc;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import javatests.DiamondIterableMapMRO;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
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
      MacroTransformationContext macroTransformationContext0 = mock(MacroTransformationContext.class, new ViolatedAssumptionAnswer());
      TocMacroParameters tocMacroParameters0 = mock(TocMacroParameters.class, new ViolatedAssumptionAnswer());
      MacroTransformationContext macroTransformationContext1 = new MacroTransformationContext();
      MacroTransformationContext macroTransformationContext2 = mock(MacroTransformationContext.class, new ViolatedAssumptionAnswer());
      DiamondIterableMapMRO diamondIterableMapMRO0 = new DiamondIterableMapMRO();
      MacroBlock macroBlock0 = new MacroBlock("[~K?AP", diamondIterableMapMRO0, "p_<q-:P", false);
      macroTransformationContext1.setCurrentMacroBlock(macroBlock0);
      MacroTransformationContext macroTransformationContext3 = macroTransformationContext1.clone();
      TreeParametersBuilder treeParametersBuilder0 = new TreeParametersBuilder();
      TocMacroParameters tocMacroParameters1 = new TocMacroParameters();
      treeParametersBuilder0.build(macroBlock0, tocMacroParameters1, macroTransformationContext2);
      TocMacroParameters tocMacroParameters2 = new TocMacroParameters();
      TocMacroParameters.Scope tocMacroParameters_Scope0 = TocMacroParameters.Scope.LOCAL;
      tocMacroParameters2.setStart(6);
      tocMacroParameters1.setScope(tocMacroParameters_Scope0);
      macroTransformationContext3.setId("XXXkO)<B");
      Block block0 = null;
      TocMacroParameters tocMacroParameters3 = new TocMacroParameters();
      treeParametersBuilder0.build((Block) null, tocMacroParameters3, macroTransformationContext3);
      // Undeclared exception!
      treeParametersBuilder0.build(macroBlock0, tocMacroParameters1, macroTransformationContext3);
  }
}
