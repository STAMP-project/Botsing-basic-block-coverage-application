/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 02:07:12 UTC 2020
 */

package org.xwiki.rendering.internal.macro.toc;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.time.ZoneId;
import java.util.Map;
import javax.inject.Provider;
import org.apache.commons.dbcp2.PoolableConnection;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;
import org.xwiki.component.embed.EmbeddableComponentManager;
import org.xwiki.rendering.block.Block;
import org.xwiki.rendering.block.ImageBlock;
import org.xwiki.rendering.block.MacroBlock;
import org.xwiki.rendering.internal.macro.toc.TreeParametersBuilder;
import org.xwiki.rendering.listener.reference.ResourceReference;
import org.xwiki.rendering.macro.toc.TocMacroParameters;
import org.xwiki.rendering.transformation.MacroTransformationContext;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class TreeParametersBuilder_ESTest extends TreeParametersBuilder_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Block block0 = mock(Block.class, new ViolatedAssumptionAnswer());
      EmbeddableComponentManager embeddableComponentManager0 = new EmbeddableComponentManager();
      TocMacroParameters tocMacroParameters0 = new TocMacroParameters();
      TocMacroParameters tocMacroParameters1 = new TocMacroParameters();
      TocMacroParameters.Scope tocMacroParameters_Scope0 = TocMacroParameters.Scope.LOCAL;
      tocMacroParameters1.setScope(tocMacroParameters_Scope0);
      tocMacroParameters1.setStart((-11));
      TreeParametersBuilder treeParametersBuilder0 = new TreeParametersBuilder();
      MacroTransformationContext macroTransformationContext0 = new MacroTransformationContext();
      Map<String, String> map0 = ZoneId.SHORT_IDS;
      MacroBlock macroBlock0 = new MacroBlock("R/}n/l-yD[S", map0, true);
      macroTransformationContext0.setCurrentMacroBlock(macroBlock0);
      ImageBlock imageBlock0 = new ImageBlock((ResourceReference) null, true);
      treeParametersBuilder0.build(imageBlock0, tocMacroParameters1, macroTransformationContext0);
      treeParametersBuilder0.build(block0, tocMacroParameters1, macroTransformationContext0);
      MacroTransformationContext macroTransformationContext1 = macroTransformationContext0.clone();
      macroTransformationContext0.getXDOM();
      Provider<PoolableConnection> provider0 = (Provider<PoolableConnection>) mock(Provider.class, new ViolatedAssumptionAnswer());
      EmbeddableComponentManager embeddableComponentManager1 = new EmbeddableComponentManager("RSn/l-y-D[S");
      TocMacroParameters tocMacroParameters2 = new TocMacroParameters();
      TocMacroParameters tocMacroParameters3 = new TocMacroParameters();
      macroTransformationContext1.setId("R/}n/l-yD[S");
      TocMacroParameters.Scope tocMacroParameters_Scope1 = TocMacroParameters.Scope.LOCAL;
      tocMacroParameters3.setScope(tocMacroParameters_Scope1);
      tocMacroParameters0.setStart((-11));
      TreeParametersBuilder treeParametersBuilder1 = new TreeParametersBuilder();
      MacroTransformationContext macroTransformationContext2 = new MacroTransformationContext();
      treeParametersBuilder0.build((Block) null, tocMacroParameters0, macroTransformationContext2);
      // Undeclared exception!
      treeParametersBuilder0.build((Block) null, tocMacroParameters3, macroTransformationContext1);
  }
}
