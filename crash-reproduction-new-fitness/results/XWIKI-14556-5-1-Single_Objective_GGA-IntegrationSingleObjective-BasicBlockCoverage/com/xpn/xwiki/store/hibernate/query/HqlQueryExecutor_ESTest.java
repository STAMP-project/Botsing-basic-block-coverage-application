/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 12:16:49 UTC 2020
 */

package com.xpn.xwiki.store.hibernate.query;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.xpn.xwiki.store.hibernate.query.HqlQueryExecutor;
import javax.inject.Provider;
import org.apache.commons.collections.ExtendedProperties;
import org.apache.commons.dbcp2.DelegatingConnection;
import org.apache.commons.dbcp2.PoolingConnection;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.hibernate.FlushMode;
import org.hibernate.engine.SessionImplementor;
import org.hibernate.engine.query.OrdinalParameterDescriptor;
import org.hibernate.engine.query.ParameterMetadata;
import org.hibernate.impl.CollectionFilterImpl;
import org.junit.runner.RunWith;
import org.xwiki.component.internal.multi.ComponentManagerManager;
import org.xwiki.context.Execution;
import org.xwiki.query.internal.EscapeLikeParametersFilter;
import org.xwiki.security.authorization.ContextualAuthorizationManager;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class HqlQueryExecutor_ESTest extends HqlQueryExecutor_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Execution execution0 = mock(Execution.class, new ViolatedAssumptionAnswer());
      Execution execution1 = mock(Execution.class, new ViolatedAssumptionAnswer());
      Provider<EscapeLikeParametersFilter> provider0 = (Provider<EscapeLikeParametersFilter>) mock(Provider.class, new ViolatedAssumptionAnswer());
      ComponentManagerManager componentManagerManager0 = mock(ComponentManagerManager.class, new ViolatedAssumptionAnswer());
      HqlQueryExecutor hqlQueryExecutor0 = new HqlQueryExecutor();
      ContextualAuthorizationManager contextualAuthorizationManager0 = mock(ContextualAuthorizationManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "authorization", (Object) contextualAuthorizationManager0);
      SessionImplementor sessionImplementor0 = mock(SessionImplementor.class, new ViolatedAssumptionAnswer());
      ExtendedProperties extendedProperties0 = new ExtendedProperties();
      ParameterMetadata parameterMetadata0 = new ParameterMetadata((OrdinalParameterDescriptor[]) null, extendedProperties0);
      CollectionFilterImpl collectionFilterImpl0 = new CollectionFilterImpl(",&#104;ttp://", componentManagerManager0, sessionImplementor0, parameterMetadata0);
      FlushMode flushMode0 = FlushMode.NEVER;
      collectionFilterImpl0.setFlushMode(flushMode0);
      DelegatingConnection<PoolingConnection> delegatingConnection0 = new DelegatingConnection<PoolingConnection>((PoolingConnection) null);
      delegatingConnection0.getInnermostDelegate();
      // Undeclared exception!
      hqlQueryExecutor0.setNamedParameter(collectionFilterImpl0, "\"c;9E6[y", ",&#104;ttp://");
  }
}
