/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 17:20:38 UTC 2020
 */

package com.xpn.xwiki.internal.skin;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import ch.qos.logback.classic.Logger;
import com.xpn.xwiki.internal.ReadOnlyXWikiContextProvider;
import com.xpn.xwiki.internal.skin.EnvironmentSkin;
import com.xpn.xwiki.internal.skin.InternalSkinConfiguration;
import com.xpn.xwiki.internal.skin.InternalSkinManager;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.junit.runner.RunWith;
import org.xwiki.component.embed.EmbeddableComponentManager;
import org.xwiki.configuration.ConfigurationSource;
import org.xwiki.configuration.internal.AbstractConfigurationSourceProvider;
import org.xwiki.configuration.internal.RestrictedConfigurationSourceProvider;
import org.xwiki.context.Execution;
import org.xwiki.environment.Environment;
import org.xwiki.model.reference.DocumentReferenceResolver;
import org.xwiki.rendering.internal.syntax.DefaultSyntaxFactory;
import ucar.httpservices.CustomX509TrustManager;
import ucar.nc2.util.net.URLStreamHandlerFactory;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractSkin_ESTest extends AbstractSkin_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      InternalSkinManager internalSkinManager0 = mock(InternalSkinManager.class, new ViolatedAssumptionAnswer());
      Logger logger0 = (Logger)CustomX509TrustManager.logger;
      InternalSkinConfiguration internalSkinConfiguration0 = new InternalSkinConfiguration();
      RestrictedConfigurationSourceProvider restrictedConfigurationSourceProvider0 = new RestrictedConfigurationSourceProvider();
      EmbeddableComponentManager embeddableComponentManager0 = new EmbeddableComponentManager();
      Injector.inject(restrictedConfigurationSourceProvider0, (Class<?>) AbstractConfigurationSourceProvider.class, "componentManager", (Object) embeddableComponentManager0);
      Injector.validateBean(restrictedConfigurationSourceProvider0, (Class<?>) RestrictedConfigurationSourceProvider.class);
      ConfigurationSource configurationSource0 = restrictedConfigurationSourceProvider0.get();
      Injector.inject(internalSkinConfiguration0, (Class<?>) InternalSkinConfiguration.class, "xwikicfg", (Object) configurationSource0);
      Injector.validateBean(internalSkinConfiguration0, (Class<?>) InternalSkinConfiguration.class);
      DocumentReferenceResolver<Object> documentReferenceResolver0 = (DocumentReferenceResolver<Object>) mock(DocumentReferenceResolver.class, new ViolatedAssumptionAnswer());
      Logger logger1 = (Logger)URLStreamHandlerFactory.log;
      ReadOnlyXWikiContextProvider readOnlyXWikiContextProvider0 = new ReadOnlyXWikiContextProvider();
      Execution execution0 = mock(Execution.class, new ViolatedAssumptionAnswer());
      Injector.inject(readOnlyXWikiContextProvider0, (Class<?>) ReadOnlyXWikiContextProvider.class, "execution", (Object) execution0);
      Injector.validateBean(readOnlyXWikiContextProvider0, (Class<?>) ReadOnlyXWikiContextProvider.class);
      DefaultSyntaxFactory defaultSyntaxFactory0 = new DefaultSyntaxFactory();
      EnvironmentSkin environmentSkin0 = new EnvironmentSkin("a%e>9v*58RY(?;", internalSkinManager0, internalSkinConfiguration0, logger1, defaultSyntaxFactory0, (Environment) null, readOnlyXWikiContextProvider0);
      environmentSkin0.createResource("", "a%e>9v*58RY(?;");
      // Undeclared exception!
      environmentSkin0.getOutputSyntax();
  }
}
