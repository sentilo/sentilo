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
package org.sentilo.common.enums;

/**
 * Sentilo internal signal types, used to communicate actions to be executed by some modules.
 * <ul>
 * <li>DELETE_ENTITIES: signal published when providers are deleted from Redis (an associated action
 * could be invalidated some local caches, .... ).
 * <li>DELETE_SENSORS: signal published when sensors are deleted from Redis.
 * <li>DELETE_ALERTS: signal published when alerts are deleted from Redis.
 * <li>RELOAD_ENTITIES: signal published to communicate to API Server instances that must reload
 * credentials and permissions caches from Catalog
 * <li>RELOAD_SUBSCRIPTIONS: signal published to communicate to API Server instances that must
 * reload subscriptions from Redis to synchronize all instances.
 * <li>RELOAD_ALERTS: signal published to communicate to sentilo-agent-alert instances that must
 * reload internal alerts from MongoDB
 *
 * </ul>
 *
 */
public enum SignalType {
  DELETE_ENTITIES, DELETE_SENSORS, DELETE_ALERTS, RELOAD_ENTITIES, RELOAD_SUBSCRIPTIONS, RELOAD_ALERTS;
}
