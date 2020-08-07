/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 05:51:26 UTC 2020
 */

package org.xwiki.rendering.internal.macro.toc;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Map;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;
import org.xwiki.rendering.block.Block;
import org.xwiki.rendering.block.MacroBlock;
import org.xwiki.rendering.internal.macro.toc.TreeParametersBuilder;
import org.xwiki.rendering.listener.Listener;
import org.xwiki.rendering.macro.toc.TocMacroParameters;
import org.xwiki.rendering.transformation.MacroTransformationContext;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class TreeParametersBuilder_ESTest extends TreeParametersBuilder_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Block block0 = mock(Block.class, new ViolatedAssumptionAnswer());
      MacroTransformationContext macroTransformationContext0 = new MacroTransformationContext();
      MacroTransformationContext macroTransformationContext1 = macroTransformationContext0.clone();
      macroTransformationContext0.clone();
      TocMacroParameters tocMacroParameters0 = new TocMacroParameters();
      Map<String, String> map0 = Listener.EMPTY_PARAMETERS;
      MacroBlock macroBlock0 = new MacroBlock("\r", map0, "E]Elet*>F#5,T$8aC9", false);
      macroTransformationContext1.setCurrentMacroBlock(macroBlock0);
      macroTransformationContext0.setInline(true);
      TocMacroParameters.Scope tocMacroParameters_Scope0 = TocMacroParameters.Scope.PAGE;
      tocMacroParameters0.setScope(tocMacroParameters_Scope0);
      TocMacroParameters.Scope tocMacroParameters_Scope1 = TocMacroParameters.Scope.LOCAL;
      tocMacroParameters0.setScope(tocMacroParameters_Scope1);
      TreeParametersBuilder treeParametersBuilder0 = new TreeParametersBuilder();
      MacroTransformationContext macroTransformationContext2 = new MacroTransformationContext();
      TocMacroParameters.Scope tocMacroParameters_Scope2 = TocMacroParameters.Scope.LOCAL;
      tocMacroParameters0.setScope(tocMacroParameters_Scope2);
      TreeParametersBuilder treeParametersBuilder1 = new TreeParametersBuilder();
      // Undeclared exception!
      treeParametersBuilder1.build(block0, tocMacroParameters0, macroTransformationContext1);
  }
}
