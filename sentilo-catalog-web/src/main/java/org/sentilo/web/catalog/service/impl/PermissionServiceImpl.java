/*
 * Sentilo
 *   
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de  Barcelona.
 *   
 * This program is licensed and may be used, modified and redistributed under the
 * terms  of the European Public License (EUPL), either version 1.1 or (at your 
 * option) any later version as soon as they are approved by the European 
 * Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms
 * of the GNU Lesser General Public License as published by the Free Software 
 * Foundation; either  version 3 of the License, or (at your option) any later 
 * version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR 
 * CONDITIONS OF ANY KIND, either express or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations 
 * and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along 
 * with this program; if not, you may find them at: 
 *   
 *   https://joinup.ec.europa.eu/software/page/eupl/licence-eupl
 *   http://www.gnu.org/licenses/ 
 *   and 
 *   https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.web.catalog.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.sentilo.common.domain.AuthorizedProvider;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.web.catalog.converter.PermissionConverter;
import org.sentilo.web.catalog.converter.PlatformMessageConverter;
import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.Permission;
import org.sentilo.web.catalog.domain.Permissions;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.exception.builder.CompoundDuplicateKeyExceptionBuilder;
import org.sentilo.web.catalog.repository.PermissionRepository;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.ComponentService;
import org.sentilo.web.catalog.service.PermissionService;
import org.sentilo.web.catalog.service.SensorService;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.validator.DefaultEntityKeyValidatorImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;


@Service
public class PermissionServiceImpl extends AbstractBaseServiceImpl<Permission> implements PermissionService {

	@Autowired
	private PermissionRepository repository;
	
	@Autowired
	private SensorService sensorService;
	
	@Autowired
	private ComponentService componentService;

	@Value("${catalog.app.id}")
	private String catalogApplicationId;

	public PermissionServiceImpl() {
		super(Permission.class);
	}

	@Override
	protected void doAfterInit() {
		this.entityKeyValidator = new DefaultEntityKeyValidatorImpl(getRepository(), new CompoundDuplicateKeyExceptionBuilder("error.permission.duplicate.key"));
		super.doAfterInit();
	}

	@Override
	public PermissionRepository getRepository() {
		return repository;
	}

	@Override
	public String getEntityId(Permission entity) {
		return entity.getId();
	}

	/*
	 * (non-Javadoc)
	 * @see org.sentilo.web.catalog.service.PermissionService#retrievePermissions()
	 */
	public Permissions retrievePermissions() {
		return new Permissions(findAll());
	}

	/*
	 * (non-Javadoc)
	 * @see org.sentilo.web.catalog.service.PermissionService#deleteRelated(org.sentilo.web.catalog.domain.CatalogDocument)
	 */
	public void deleteRelated(CatalogDocument entity) {
		// Tenemos que eliminar todos aquellos permisos en donde el target o el source del permiso sea la entidad informada
		// por parametro.
		SearchFilter filter = new SearchFilter();
		filter.addParam("source", entity.getId());
		filter.addParam("target", entity.getId());
		
		getMongoOps().remove(buildQuery(filter), Permission.class);				
	}

	/*
	 * (non-Javadoc)
	 * @see org.sentilo.web.catalog.service.PermissionService#createRelated(org.sentilo.web.catalog.domain.CatalogDocument)
	 */
	public void createRelated(CatalogDocument entity) {
		createOwnPermission(entity);
		createCatalogPermission(entity);
	}
	
	/*
	 * (non-Javadoc)
	 * @see org.sentilo.web.catalog.service.PermissionService#getAuthorizedProviders(java.lang.String, java.lang.String)
	 */
	public List<AuthorizedProvider> getAuthorizedProviders(String entityId, String sensorType){
		
		//TODO Mikel: Este metodo no parece encajar muy bien en este servicio. Replantearse localizacion
		List<AuthorizedProvider> authorizedProviders = new ArrayList<AuthorizedProvider>();
				
		SearchFilter filter = new SearchFilter();
		filter.addParam("source", entityId);		
		List<Permission> result = super.search(filter).getContent();
		
		Map<String, List<Component>> mapComponents = new HashMap<String, List<Component>>(); 
		//Para cada permiso, hemos de definir un nuevo objeto AuthorizedProvider con su lista de sensores
		for(Permission permission: result){						
			List<Sensor> sensors = getSensorsByProviderAndType(permission.getTarget(), sensorType);
			if(!CollectionUtils.isEmpty(sensors)){				
				List<Component> components = getComponentsByProvider(permission.getTarget(), mapComponents);			
				List<CatalogSensor> catalogSensors = PlatformMessageConverter.convertToCatalogSensorList(sensors, components); 						
				authorizedProviders.add(new AuthorizedProvider(permission.getTarget(), permission.getType().toString(),	catalogSensors));
			}
		}
						
		return authorizedProviders;
	}
	
	private List<Sensor> getSensorsByProviderAndType(String providerId, String sensorType){
		SearchFilter filter = new SearchFilter();
		filter.addAndParam("providerId", providerId);
		if(StringUtils.hasText(sensorType)){
			filter.addAndParam("type", sensorType);
		}
								
		return sensorService.search(filter).getContent();
	}
	
	private List<Component> getComponentsByProvider(String providerId, Map<String, List<Component>> mapComponents){
		
		List<Component> components = mapComponents.get(providerId);
		if(components == null){
			SearchFilter filter = new SearchFilter();
			filter.addAndParam("providerId", providerId);		
									
			components = componentService.search(filter).getContent();
			mapComponents.put(providerId, components);
		}
		
		return components;
	}

	private void createCatalogPermission(CatalogDocument entity) {
		create(buildCatalogPermissionFor(entity));
	}

	private void createOwnPermission(CatalogDocument entity) {
		create(PermissionConverter.fromCatalogDocument(entity));
	}

	private Permission buildCatalogPermissionFor(CatalogDocument entity) {
		return new Permission(catalogApplicationId, entity.getId(), Constants.CATALOG_PERMISSION_TYPE);
	}
}
