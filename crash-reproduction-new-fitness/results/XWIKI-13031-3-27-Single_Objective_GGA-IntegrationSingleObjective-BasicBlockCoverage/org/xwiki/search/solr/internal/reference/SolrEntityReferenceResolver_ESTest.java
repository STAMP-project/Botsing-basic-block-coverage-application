/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 19:06:45 UTC 2020
 */

package org.xwiki.search.solr.internal.reference;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.solr.common.SolrDocument;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.junit.runner.RunWith;
import org.xwiki.model.EntityType;
import org.xwiki.model.internal.reference.DefaultReferenceEntityReferenceResolver;
import org.xwiki.model.internal.reference.DefaultStringEntityReferenceResolver;
import org.xwiki.model.reference.EntityReferenceProvider;
import org.xwiki.model.reference.EntityReferenceResolver;
import org.xwiki.search.solr.internal.reference.SolrEntityReferenceResolver;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class SolrEntityReferenceResolver_ESTest extends SolrEntityReferenceResolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DefaultReferenceEntityReferenceResolver defaultReferenceEntityReferenceResolver0 = new DefaultReferenceEntityReferenceResolver();
      DefaultStringEntityReferenceResolver defaultStringEntityReferenceResolver0 = new DefaultStringEntityReferenceResolver();
      EntityReferenceProvider entityReferenceProvider0 = mock(EntityReferenceProvider.class, new ViolatedAssumptionAnswer());
      EntityType entityType0 = EntityType.OBJECT_PROPERTY;
      SolrEntityReferenceResolver solrEntityReferenceResolver0 = new SolrEntityReferenceResolver();
      EntityReferenceResolver<Integer> entityReferenceResolver0 = (EntityReferenceResolver<Integer>) mock(EntityReferenceResolver.class, new ViolatedAssumptionAnswer());
      SolrDocument solrDocument0 = new SolrDocument();
      String string0 = "wiki";
      DefaultStringEntityReferenceResolver defaultStringEntityReferenceResolver1 = new DefaultStringEntityReferenceResolver();
      EntityReferenceProvider entityReferenceProvider1 = mock(EntityReferenceProvider.class, new ViolatedAssumptionAnswer());
      Injector.inject(defaultStringEntityReferenceResolver1, (Class<?>) DefaultStringEntityReferenceResolver.class, "provider", (Object) entityReferenceProvider1);
      Injector.validateBean(defaultStringEntityReferenceResolver1, (Class<?>) DefaultStringEntityReferenceResolver.class);
      solrDocument0.put("wiki", (Object) defaultStringEntityReferenceResolver1);
      // Undeclared exception!
      solrEntityReferenceResolver0.resolve(solrDocument0, entityType0, (Object[]) null);
  }
}
