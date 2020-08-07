/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 15:55:17 UTC 2020
 */

package com.xpn.xwiki.store.hibernate.query;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.xpn.xwiki.doc.merge.MergeResult;
import com.xpn.xwiki.store.hibernate.HibernateSessionFactory;
import com.xpn.xwiki.store.hibernate.query.HqlQueryExecutor;
import java.awt.Panel;
import java.util.Map;
import javax.inject.Provider;
import org.apache.commons.collections.ExtendedProperties;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.hibernate.LockMode;
import org.hibernate.LockOptions;
import org.hibernate.Query;
import org.hibernate.engine.SessionImplementor;
import org.hibernate.engine.query.OrdinalParameterDescriptor;
import org.hibernate.engine.query.ParameterMetadata;
import org.hibernate.impl.CollectionFilterImpl;
import org.junit.runner.RunWith;
import org.xwiki.component.embed.EmbeddableComponentManager;
import org.xwiki.component.internal.AbstractEntityComponentManager;
import org.xwiki.component.internal.ContextComponentManager;
import org.xwiki.component.internal.UserComponentManager;
import org.xwiki.component.internal.WikiComponentManager;
import org.xwiki.component.internal.multi.AbstractGenericComponentManager;
import org.xwiki.component.internal.multi.ComponentManagerManager;
import org.xwiki.context.Execution;
import org.xwiki.job.event.status.JobProgressManager;
import org.xwiki.model.reference.EntityReferenceSerializer;
import org.xwiki.query.internal.DefaultQuery;
import org.xwiki.security.authorization.ContextualAuthorizationManager;
import org.xwiki.wiki.descriptor.WikiDescriptorManager;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class HqlQueryExecutor_ESTest extends HqlQueryExecutor_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      ContextualAuthorizationManager contextualAuthorizationManager0 = mock(ContextualAuthorizationManager.class, new ViolatedAssumptionAnswer());
      Provider<ContextComponentManager> provider0 = (Provider<ContextComponentManager>) mock(Provider.class, new ViolatedAssumptionAnswer());
      HibernateSessionFactory hibernateSessionFactory0 = mock(HibernateSessionFactory.class, new ViolatedAssumptionAnswer());
      Query query0 = mock(Query.class, new ViolatedAssumptionAnswer());
      Query query1 = mock(Query.class, new ViolatedAssumptionAnswer());
      ContextualAuthorizationManager contextualAuthorizationManager1 = mock(ContextualAuthorizationManager.class, new ViolatedAssumptionAnswer());
      Provider<UserComponentManager> provider1 = (Provider<UserComponentManager>) mock(Provider.class, new ViolatedAssumptionAnswer());
      Execution execution0 = mock(Execution.class, new ViolatedAssumptionAnswer());
      HqlQueryExecutor hqlQueryExecutor0 = new HqlQueryExecutor();
      ContextualAuthorizationManager contextualAuthorizationManager2 = mock(ContextualAuthorizationManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "authorization", (Object) contextualAuthorizationManager2);
      Provider<Panel> provider2 = (Provider<Panel>) mock(Provider.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "componentManagerProvider", (Object) provider2);
      Execution execution1 = mock(Execution.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "execution", (Object) execution1);
      JobProgressManager jobProgressManager0 = mock(JobProgressManager.class, new ViolatedAssumptionAnswer());
      MergeResult mergeResult0 = new MergeResult();
      DefaultQuery defaultQuery0 = new DefaultQuery("", hqlQueryExecutor0);
      defaultQuery0.checkCurrentUser(false);
      WikiComponentManager wikiComponentManager0 = new WikiComponentManager();
      EmbeddableComponentManager embeddableComponentManager0 = new EmbeddableComponentManager("");
      Injector.inject(wikiComponentManager0, (Class<?>) WikiComponentManager.class, "rootComponentManager", (Object) embeddableComponentManager0);
      WikiDescriptorManager wikiDescriptorManager0 = mock(WikiDescriptorManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(wikiComponentManager0, (Class<?>) WikiComponentManager.class, "wikis", (Object) wikiDescriptorManager0);
      Execution execution2 = mock(Execution.class, new ViolatedAssumptionAnswer());
      Injector.inject(wikiComponentManager0, (Class<?>) AbstractEntityComponentManager.class, "execution", (Object) execution2);
      EntityReferenceSerializer<String> entityReferenceSerializer0 = (EntityReferenceSerializer<String>) mock(EntityReferenceSerializer.class, new ViolatedAssumptionAnswer());
      Injector.inject(wikiComponentManager0, (Class<?>) AbstractEntityComponentManager.class, "serializer", (Object) entityReferenceSerializer0);
      ComponentManagerManager componentManagerManager0 = mock(ComponentManagerManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(wikiComponentManager0, (Class<?>) AbstractGenericComponentManager.class, "componentManagerManager", (Object) componentManagerManager0);
      Injector.validateBean(wikiComponentManager0, (Class<?>) WikiComponentManager.class);
      SessionImplementor sessionImplementor0 = mock(SessionImplementor.class, new ViolatedAssumptionAnswer());
      OrdinalParameterDescriptor[] ordinalParameterDescriptorArray0 = new OrdinalParameterDescriptor[0];
      ExtendedProperties extendedProperties0 = new ExtendedProperties();
      extendedProperties0.subset("");
      ParameterMetadata parameterMetadata0 = new ParameterMetadata(ordinalParameterDescriptorArray0, (Map) null);
      CollectionFilterImpl collectionFilterImpl0 = new CollectionFilterImpl("", wikiComponentManager0, sessionImplementor0, parameterMetadata0);
      LockOptions lockOptions0 = LockOptions.NONE;
      LockMode lockMode0 = LockMode.OPTIMISTIC;
      LockOptions lockOptions1 = lockOptions0.setLockMode(lockMode0);
      Query query2 = collectionFilterImpl0.setLockOptions(lockOptions1);
      // Undeclared exception!
      hqlQueryExecutor0.setNamedParameter(query2, "", provider0);
  }
}
