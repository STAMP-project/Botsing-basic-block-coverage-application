/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 19:07:38 UTC 2020
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
import org.xwiki.model.internal.reference.DefaultStringEntityReferenceResolver;
import org.xwiki.model.internal.reference.ExplicitStringEntityReferenceResolver;
import org.xwiki.model.reference.EntityReferenceProvider;
import org.xwiki.model.reference.EntityReferenceResolver;
import org.xwiki.search.solr.internal.reference.SolrEntityReferenceResolver;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class SolrEntityReferenceResolver_ESTest extends SolrEntityReferenceResolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DefaultStringEntityReferenceResolver defaultStringEntityReferenceResolver0 = new DefaultStringEntityReferenceResolver();
      EntityReferenceProvider entityReferenceProvider0 = mock(EntityReferenceProvider.class, new ViolatedAssumptionAnswer());
      EntityReferenceResolver<Integer> entityReferenceResolver0 = (EntityReferenceResolver<Integer>) mock(EntityReferenceResolver.class, new ViolatedAssumptionAnswer());
      SolrDocument solrDocument0 = new SolrDocument();
      SolrDocument solrDocument1 = new SolrDocument();
      SolrDocument solrDocument2 = new SolrDocument();
      Object object0 = new Object();
      solrDocument2.putIfAbsent("wiki", object0);
      SolrEntityReferenceResolver solrEntityReferenceResolver0 = new SolrEntityReferenceResolver();
      EntityReferenceResolver<ExplicitStringEntityReferenceResolver> entityReferenceResolver1 = (EntityReferenceResolver<ExplicitStringEntityReferenceResolver>) mock(EntityReferenceResolver.class, new ViolatedAssumptionAnswer());
      Injector.inject(solrEntityReferenceResolver0, (Class<?>) SolrEntityReferenceResolver.class, "explicitReferenceEntityReferenceResolver", (Object) entityReferenceResolver1);
      Injector.validateBean(solrEntityReferenceResolver0, (Class<?>) SolrEntityReferenceResolver.class);
      SolrDocument solrDocument3 = new SolrDocument();
      SolrEntityReferenceResolver solrEntityReferenceResolver1 = new SolrEntityReferenceResolver();
      Injector.inject(solrEntityReferenceResolver1, (Class<?>) SolrEntityReferenceResolver.class, "explicitReferenceEntityReferenceResolver", (Object) entityReferenceResolver0);
      Injector.validateBean(solrEntityReferenceResolver1, (Class<?>) SolrEntityReferenceResolver.class);
      EntityType entityType0 = EntityType.BLOCK;
      // Undeclared exception!
      solrEntityReferenceResolver0.resolve(solrDocument2, entityType0, (Object[]) null);
  }
}
