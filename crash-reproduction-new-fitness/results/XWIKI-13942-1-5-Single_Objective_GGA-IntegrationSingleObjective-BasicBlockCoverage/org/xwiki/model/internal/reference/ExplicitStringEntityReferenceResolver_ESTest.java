/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 12:50:36 UTC 2020
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
import org.xwiki.model.internal.reference.ExplicitStringEntityReferenceResolver;
import org.xwiki.model.internal.reference.SymbolScheme;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class ExplicitStringEntityReferenceResolver_ESTest extends ExplicitStringEntityReferenceResolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      ExplicitStringEntityReferenceResolver explicitStringEntityReferenceResolver0 = new ExplicitStringEntityReferenceResolver();
      SymbolScheme symbolScheme0 = mock(SymbolScheme.class, new ViolatedAssumptionAnswer());
      Injector.inject(explicitStringEntityReferenceResolver0, (Class<?>) AbstractStringEntityReferenceResolver.class, "symbolScheme", (Object) symbolScheme0);
      Injector.validateBean(explicitStringEntityReferenceResolver0, (Class<?>) ExplicitStringEntityReferenceResolver.class);
      EntityType entityType0 = EntityType.ATTACHMENT;
      Object[] objectArray0 = new Object[5];
      objectArray0[0] = (Object) explicitStringEntityReferenceResolver0;
      objectArray0[1] = (Object) explicitStringEntityReferenceResolver0;
      objectArray0[2] = (Object) entityType0;
      objectArray0[3] = objectArray0[2];
      objectArray0[4] = (Object) symbolScheme0;
      // Undeclared exception!
      explicitStringEntityReferenceResolver0.resolveDefaultReference(entityType0, objectArray0);
  }
}
