/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 16:13:50 UTC 2020
 */

package org.xwiki.rendering.internal.parser;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import javax.inject.Provider;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.junit.runner.RunWith;
import org.xwiki.component.internal.ContextComponentManager;
import org.xwiki.model.reference.EntityReferenceSerializer;
import org.xwiki.rendering.internal.parser.DefaultContentParser;
import org.xwiki.rendering.internal.parser.confluence.ConfluenceParser;
import org.xwiki.rendering.syntax.Syntax;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class DefaultContentParser_ESTest extends DefaultContentParser_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DefaultContentParser defaultContentParser0 = new DefaultContentParser();
      Provider<ConfluenceParser> provider0 = (Provider<ConfluenceParser>) mock(Provider.class, new ViolatedAssumptionAnswer());
      doReturn((Object) null).when(provider0).get();
      Injector.inject(defaultContentParser0, (Class<?>) DefaultContentParser.class, "componentManagerProvider", (Object) provider0);
      EntityReferenceSerializer<Integer> entityReferenceSerializer0 = (EntityReferenceSerializer<Integer>) mock(EntityReferenceSerializer.class, new ViolatedAssumptionAnswer());
      Injector.inject(defaultContentParser0, (Class<?>) DefaultContentParser.class, "serializer", (Object) entityReferenceSerializer0);
      Injector.validateBean(defaultContentParser0, (Class<?>) DefaultContentParser.class);
      DefaultContentParser defaultContentParser1 = new DefaultContentParser();
      Provider<ContextComponentManager> provider1 = (Provider<ContextComponentManager>) mock(Provider.class, new ViolatedAssumptionAnswer());
      Injector.inject(defaultContentParser1, (Class<?>) DefaultContentParser.class, "componentManagerProvider", (Object) provider1);
      EntityReferenceSerializer<ConfluenceParser> entityReferenceSerializer1 = (EntityReferenceSerializer<ConfluenceParser>) mock(EntityReferenceSerializer.class, new ViolatedAssumptionAnswer());
      Injector.inject(defaultContentParser1, (Class<?>) DefaultContentParser.class, "serializer", (Object) entityReferenceSerializer1);
      Injector.validateBean(defaultContentParser1, (Class<?>) DefaultContentParser.class);
      Syntax syntax0 = Syntax.TEX_1_0;
      // Undeclared exception!
      defaultContentParser0.parse("VW8 ;yGl8|U", syntax0);
  }
}
