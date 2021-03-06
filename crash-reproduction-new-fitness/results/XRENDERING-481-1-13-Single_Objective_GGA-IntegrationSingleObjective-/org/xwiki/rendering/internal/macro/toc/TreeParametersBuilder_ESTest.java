/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 02:40:15 UTC 2020
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
import org.xwiki.rendering.block.GroupBlock;
import org.xwiki.rendering.block.MacroBlock;
import org.xwiki.rendering.block.XDOM;
import org.xwiki.rendering.internal.macro.toc.TreeParameters;
import org.xwiki.rendering.internal.macro.toc.TreeParametersBuilder;
import org.xwiki.rendering.listener.Listener;
import org.xwiki.rendering.macro.toc.TocMacroParameters;
import org.xwiki.rendering.syntax.Syntax;
import org.xwiki.rendering.transformation.MacroTransformationContext;
import org.xwiki.rendering.transformation.TransformationContext;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class TreeParametersBuilder_ESTest extends TreeParametersBuilder_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Block block0 = mock(Block.class, new ViolatedAssumptionAnswer());
      MacroTransformationContext macroTransformationContext0 = mock(MacroTransformationContext.class, new ViolatedAssumptionAnswer());
      TreeParametersBuilder treeParametersBuilder0 = new TreeParametersBuilder();
      TocMacroParameters tocMacroParameters0 = new TocMacroParameters();
      TreeParameters treeParameters0 = treeParametersBuilder0.build(block0, tocMacroParameters0, macroTransformationContext0);
      TransformationContext transformationContext0 = new TransformationContext((XDOM) null, (Syntax) null);
      MacroTransformationContext macroTransformationContext1 = new MacroTransformationContext(transformationContext0);
      TreeParameters treeParameters1 = treeParametersBuilder0.build(treeParameters0.rootBlock, tocMacroParameters0, macroTransformationContext1);
      treeParametersBuilder0.build(treeParameters0.rootBlock, tocMacroParameters0, macroTransformationContext1);
      MacroTransformationContext macroTransformationContext2 = new MacroTransformationContext();
      treeParametersBuilder0.build(treeParameters1.rootBlock, tocMacroParameters0, macroTransformationContext1);
      TocMacroParameters.Scope tocMacroParameters_Scope0 = TocMacroParameters.Scope.LOCAL;
      tocMacroParameters0.setScope(tocMacroParameters_Scope0);
      GroupBlock groupBlock0 = new GroupBlock();
      TocMacroParameters tocMacroParameters1 = new TocMacroParameters();
      TreeParametersBuilder treeParametersBuilder1 = new TreeParametersBuilder();
      Map<String, String> map0 = Listener.EMPTY_PARAMETERS;
      MacroBlock macroBlock0 = new MacroBlock("M", map0, ".(}BFaR}m~@/ndn#yb", true);
      macroTransformationContext2.setCurrentMacroBlock(macroBlock0);
      TocMacroParameters tocMacroParameters2 = new TocMacroParameters();
      // Undeclared exception!
      treeParametersBuilder1.build(groupBlock0, tocMacroParameters0, macroTransformationContext2);
  }
}
