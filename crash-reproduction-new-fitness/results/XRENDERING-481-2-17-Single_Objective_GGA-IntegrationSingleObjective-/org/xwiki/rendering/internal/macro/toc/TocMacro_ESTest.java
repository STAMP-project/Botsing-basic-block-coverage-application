/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 05:36:45 UTC 2020
 */

package org.xwiki.rendering.internal.macro.toc;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.ArrayList;
import java.util.HashMap;
import javax.inject.Provider;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.junit.runner.RunWith;
import org.xwiki.component.descriptor.ComponentDescriptor;
import org.xwiki.properties.BeanManager;
import org.xwiki.rendering.block.DefinitionTermBlock;
import org.xwiki.rendering.block.MacroBlock;
import org.xwiki.rendering.block.TableBlock;
import org.xwiki.rendering.block.XDOM;
import org.xwiki.rendering.internal.macro.toc.TocMacro;
import org.xwiki.rendering.internal.renderer.DefaultLinkLabelGenerator;
import org.xwiki.rendering.listener.MetaData;
import org.xwiki.rendering.macro.AbstractMacro;
import org.xwiki.rendering.macro.toc.TocMacroParameters;
import org.xwiki.rendering.parser.Parser;
import org.xwiki.rendering.transformation.MacroTransformationContext;
import org.xwiki.rendering.util.IdGenerator;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class TocMacro_ESTest extends TocMacro_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DefaultLinkLabelGenerator defaultLinkLabelGenerator0 = new DefaultLinkLabelGenerator();
      Parser parser0 = mock(Parser.class, new ViolatedAssumptionAnswer());
      BeanManager beanManager0 = mock(BeanManager.class, new ViolatedAssumptionAnswer());
      ComponentDescriptor<DefinitionTermBlock> componentDescriptor0 = (ComponentDescriptor<DefinitionTermBlock>) mock(ComponentDescriptor.class, new ViolatedAssumptionAnswer());
      MacroTransformationContext macroTransformationContext0 = new MacroTransformationContext();
      ArrayList<TableBlock> arrayList0 = new ArrayList<TableBlock>();
      IdGenerator idGenerator0 = new IdGenerator();
      MetaData metaData0 = new MetaData();
      XDOM xDOM0 = new XDOM(arrayList0, idGenerator0, metaData0);
      macroTransformationContext0.setXDOM(xDOM0);
      TocMacroParameters.Scope tocMacroParameters_Scope0 = TocMacroParameters.Scope.LOCAL;
      MacroTransformationContext macroTransformationContext1 = new MacroTransformationContext();
      TocMacroParameters tocMacroParameters0 = new TocMacroParameters();
      TocMacro tocMacro0 = new TocMacro();
      Injector.inject(tocMacro0, (Class<?>) TocMacro.class, "linkLabelGenerator", (Object) defaultLinkLabelGenerator0);
      macroTransformationContext0.setXDOM((XDOM) null);
      Injector.inject(tocMacro0, (Class<?>) TocMacro.class, "plainTextParser", (Object) parser0);
      Provider<XDOM> provider0 = (Provider<XDOM>) mock(Provider.class, new ViolatedAssumptionAnswer());
      doReturn((Object) null).when(provider0).get();
      Injector.inject(tocMacro0, (Class<?>) TocMacro.class, "wikiModelProvider", (Object) provider0);
      HashMap<String, String> hashMap0 = new HashMap<String, String>();
      MacroBlock macroBlock0 = new MacroBlock("The \"reference\" parameter can only be used when a WikiModel implementation is available", hashMap0, true);
      macroTransformationContext0.setCurrentMacroBlock(macroBlock0);
      macroTransformationContext1.setInline(true);
      tocMacroParameters0.setScope(tocMacroParameters_Scope0);
      Injector.inject(tocMacro0, (Class<?>) AbstractMacro.class, "beanManager", (Object) beanManager0);
      Injector.inject(tocMacro0, (Class<?>) AbstractMacro.class, "componentDescriptor", (Object) componentDescriptor0);
      Injector.validateBean(tocMacro0, (Class<?>) TocMacro.class);
      // Undeclared exception!
      tocMacro0.execute(tocMacroParameters0, "Generates a Table Of Contents.", macroTransformationContext0);
  }
}
