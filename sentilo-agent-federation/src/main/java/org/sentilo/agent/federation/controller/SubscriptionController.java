/*
 * Sentilo
 * 
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de
 * Barcelona. Modified by Opentrends adding support for multitenant deployments and SaaS.
 * Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 *
 * 
 * This program is licensed and may be used, modified and redistributed under the terms of the
 * European Public License (EUPL), either version 1.1 or (at your option) any later version as soon
 * as they are approved by the European Commission.
 * 
 * Alternatively, you may redistribute and/or modify this program under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation; either version 3 of the
 * License, or (at your option) any later version.
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied.
 * 
 * See the licenses for the specific language governing permissions, limitations and more details.
 * 
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along with this program;
 * if not, you may find them at:
 * 
 * https://joinup.ec.europa.eu/software/page/eupl/licence-eupl http://www.gnu.org/licenses/ and
 * https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.agent.federation.controller;

import javax.servlet.http.HttpServletRequest;

import org.sentilo.agent.federation.domain.FederationConfig;
import org.sentilo.agent.federation.service.LocalPlatformService;
import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.exception.RESTClientException;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.common.rest.RequestContext;
import org.sentilo.common.rest.hmac.HMACBuilder;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.platform.client.core.domain.Observation;
import org.sentilo.platform.client.core.domain.SensorObservations;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

@Controller
public class SubscriptionController {

  private static final Logger LOGGER = LoggerFactory.getLogger(SubscriptionController.class);

  public static final String MULTITENANT_ENTITY_ID_PREPEND_TOKEN = "@";
  private final StringMessageConverter converter = new DefaultStringMessageConverter();

  @Autowired
  @Qualifier("localRestClient")
  private RESTClient localRestClient;

  @Autowired
  private LocalPlatformService localPlatformService;

  @Value("${federation.subscription.secret.key.callback}")
  private String secretKey;

  @Value("${federation.subscription.endpoint}")
  private String endpoint;

  @ResponseBody
  @RequestMapping(value = "/data/federated/{federatedId}", produces = MediaType.APPLICATION_JSON_VALUE, method = RequestMethod.POST)
  public ResponseEntity<String> subscription(@RequestBody final EventMessage message, @PathVariable final String federatedId,
      final HttpServletRequest request) {
    final FederationConfig fConfig = localPlatformService.getFederatedConfig(federatedId);
    return checkRequest(fConfig, message, request) ? publishEvent(message, fConfig) : new ResponseEntity<String>(HttpStatus.BAD_REQUEST);
  }

  private boolean checkRequest(final FederationConfig fConfig, final EventMessage message, final HttpServletRequest request) {
    LOGGER.trace("check request from federated instance: {}", fConfig != null ? fConfig.getId() : "UNKNOWN");
    return fConfig != null && verifyHmacHeaders(request, message, fConfig);
  }

  private ResponseEntity<String> publishEvent(final EventMessage event, final FederationConfig fConfig) {
    try {
      final String localProviderId = buildLocalProviderId(fConfig, event.getProvider());
      final String path = String.format("data/%s/%s", localProviderId, event.getSensor());
      final String body = eventToObservationsBody(event);
      final RequestContext rc = new RequestContext(path, body);
      rc.setIdentityToken(localPlatformService.getTokenMasterApp());

      final String response = localRestClient.put(rc);
      return new ResponseEntity<String>(response, HttpStatus.OK);
    } catch (final RESTClientException rce) {
      LOGGER.warn("An error has ocurred trying to publish an event into local Sentilo instance.", rce);
      return new ResponseEntity<String>(rce.getMessage(), HttpStatus.INTERNAL_SERVER_ERROR);
    }
  }

  private boolean verifyHmacHeaders(final HttpServletRequest request, final EventMessage message, final FederationConfig fConfig) {
    try {
      final String dateHeaderValue = request.getHeader(SentiloConstants.DATE_HEADER);
      final String hmacHeaderValue = request.getHeader(SentiloConstants.HMAC_HEADER);
      final String currentEndpoint = String.format("%s%s", endpoint, fConfig.getId());
      // To check HMAC header, event message should be serialize as JSON String
      final String requestBody = converter.marshal(message);

      final boolean result = HMACBuilder.checkHeader(hmacHeaderValue, requestBody, currentEndpoint, secretKey, dateHeaderValue);

      LOGGER.trace("Result of check HMAC header {} with date header {}, body {}, endpoint {} and secretKey {} was {}", hmacHeaderValue,
          dateHeaderValue, requestBody, currentEndpoint, secretKey, result);

      return result;
    } catch (final Exception e) {
      LOGGER.warn("An error has ocurred while checking HMAC header", e);
      return false;
    }
  }

  private String eventToObservationsBody(final EventMessage event) {
    final Observation observation = new Observation(event.getMessage(), event.getTimestamp(), event.getLocation());
    final SensorObservations sensorObs = new SensorObservations(event.getSensor());
    sensorObs.addObservation(observation);
    return converter.marshal(sensorObs);
  }

  private String buildLocalProviderId(final FederationConfig fConfig, final String remoteProviderId) {
    // @see org.sentilo.web.catalog.admin.support.RemoteFederatedResources#buildLocalProviderId
    String localProviderId = String.format("%s_%s", fConfig.getId(), remoteProviderId);
    if (StringUtils.hasText(fConfig.getTenantId())) {
      localProviderId = fConfig.getTenantId() + MULTITENANT_ENTITY_ID_PREPEND_TOKEN + localProviderId;
    }

    return localProviderId;
  }
}
