/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 12:52:42 UTC 2020
 */

package org.xwiki.model.internal.reference;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.junit.runner.RunWith;
import org.xwiki.model.EntityType;
import org.xwiki.model.internal.reference.AbstractStringEntityReferenceResolver;
import org.xwiki.model.internal.reference.DefaultSymbolScheme;
import org.xwiki.model.internal.reference.ExplicitStringEntityReferenceResolver;
import org.xwiki.model.internal.reference.RelativeStringEntityReferenceResolver;
import org.xwiki.model.internal.reference.SymbolScheme;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractStringEntityReferenceResolver_ESTest extends AbstractStringEntityReferenceResolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      RelativeStringEntityReferenceResolver relativeStringEntityReferenceResolver0 = new RelativeStringEntityReferenceResolver();
      SymbolScheme symbolScheme0 = mock(SymbolScheme.class, new ViolatedAssumptionAnswer());
      Injector.inject(relativeStringEntityReferenceResolver0, (Class<?>) AbstractStringEntityReferenceResolver.class, "symbolScheme", (Object) symbolScheme0);
      Injector.validateBean(relativeStringEntityReferenceResolver0, (Class<?>) AbstractStringEntityReferenceResolver.class);
      EntityType entityType0 = EntityType.ATTACHMENT;
      Object[] objectArray0 = new Object[4];
      objectArray0[0] = (Object) symbolScheme0;
      objectArray0[1] = (Object) relativeStringEntityReferenceResolver0;
      objectArray0[2] = (Object) relativeStringEntityReferenceResolver0;
      DefaultSymbolScheme defaultSymbolScheme0 = new DefaultSymbolScheme();
      defaultSymbolScheme0.initialize();
      ExplicitStringEntityReferenceResolver explicitStringEntityReferenceResolver0 = new ExplicitStringEntityReferenceResolver();
      DefaultSymbolScheme defaultSymbolScheme1 = new DefaultSymbolScheme();
      Injector.inject(explicitStringEntityReferenceResolver0, (Class<?>) AbstractStringEntityReferenceResolver.class, "symbolScheme", (Object) defaultSymbolScheme1);
      Injector.validateBean(explicitStringEntityReferenceResolver0, (Class<?>) ExplicitStringEntityReferenceResolver.class);
      explicitStringEntityReferenceResolver0.initialize();
      explicitStringEntityReferenceResolver0.getSymbolScheme();
      // Undeclared exception!
      explicitStringEntityReferenceResolver0.resolve("BG", entityType0, objectArray0);
  }
}
