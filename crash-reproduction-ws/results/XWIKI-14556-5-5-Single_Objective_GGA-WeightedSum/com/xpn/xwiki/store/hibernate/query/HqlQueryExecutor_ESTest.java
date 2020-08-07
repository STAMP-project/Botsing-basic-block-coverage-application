/*
 * This file was automatically generated by EvoSuite
 * Wed Apr 01 13:41:16 UTC 2020
 */

package com.xpn.xwiki.store.hibernate.query;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.xpn.xwiki.store.hibernate.HibernateSessionFactory;
import com.xpn.xwiki.store.hibernate.query.HqlQueryExecutor;
import java.util.Hashtable;
import javax.inject.Provider;
import org.apache.batik.gvt.text.GVTAttributedCharacterIterator;
import org.apache.commons.dbcp2.PoolingConnection;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.hibernate.engine.SessionImplementor;
import org.hibernate.engine.query.OrdinalParameterDescriptor;
import org.hibernate.engine.query.ParameterMetadata;
import org.hibernate.impl.CollectionFilterImpl;
import org.junit.runner.RunWith;
import org.xwiki.component.embed.EmbeddableComponentManager;
import org.xwiki.context.Execution;
import org.xwiki.job.event.status.JobProgressManager;
import org.xwiki.query.internal.ScriptQuery;
import org.xwiki.security.authorization.ContextualAuthorizationManager;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class HqlQueryExecutor_ESTest extends HqlQueryExecutor_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Boolean boolean0 = new Boolean("order by sql");
      HqlQueryExecutor hqlQueryExecutor0 = new HqlQueryExecutor();
      ContextualAuthorizationManager contextualAuthorizationManager0 = mock(ContextualAuthorizationManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "authorization", (Object) contextualAuthorizationManager0);
      Provider<ScriptQuery> provider0 = (Provider<ScriptQuery>) mock(Provider.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "componentManagerProvider", (Object) provider0);
      Execution execution0 = mock(Execution.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "execution", (Object) execution0);
      JobProgressManager jobProgressManager0 = mock(JobProgressManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "progress", (Object) jobProgressManager0);
      HibernateSessionFactory hibernateSessionFactory0 = mock(HibernateSessionFactory.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "sessionFactory", (Object) hibernateSessionFactory0);
      Injector.validateBean(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class);
      Integer integer0 = GVTAttributedCharacterIterator.TextAttribute.ADJUST_ALL;
      SessionImplementor sessionImplementor0 = mock(SessionImplementor.class, new ViolatedAssumptionAnswer());
      Hashtable<PoolingConnection, EmbeddableComponentManager> hashtable0 = new Hashtable<PoolingConnection, EmbeddableComponentManager>();
      ParameterMetadata parameterMetadata0 = new ParameterMetadata((OrdinalParameterDescriptor[]) null, hashtable0);
      CollectionFilterImpl collectionFilterImpl0 = new CollectionFilterImpl("infinispan.debugDependencies", integer0, sessionImplementor0, parameterMetadata0);
      // Undeclared exception!
      hqlQueryExecutor0.setNamedParameter(collectionFilterImpl0, "6{C\ngn\"y^ZQ'^", boolean0);
  }
}
