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
package org.sentilo.web.demo.common.domain;

import java.io.Serializable;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;

import org.hibernate.validator.constraints.NotBlank;
import org.sentilo.web.demo.common.utils.Constants;
import org.springframework.format.annotation.NumberFormat;
import org.springframework.format.annotation.NumberFormat.Style;


/**
 * 
 * Representa un sensor
 *
 */
public class VirtualSensor implements Serializable {

	private static final long serialVersionUID = 1L;

	@NotBlank
	@Pattern(regexp = Constants.VALIDATION_ENTITY_NAME_REGEXP)
	private String sensorId;
	@NotBlank
	private String providerId;
	@NotBlank
	private String componentId;
	@NotNull
	private ComponentType componentType;
	@NotBlank
	private String description;
	private DataType dataType;
	@NotNull
	private Type type;
	@NotBlank
	private String unit;
	@NotBlank
	private String tokenAuth;
	@NotBlank
	@NumberFormat
    @Min(1)
	private String numOfIterations;
	@NotBlank
	@NumberFormat(style = Style.NUMBER)
    @Min(5000)
	private String freq;
	@NotBlank
	private String value;

	@Pattern(regexp = "^$|([+-]?\\d+\\.?\\d+)\\s* \\s*([+-]?\\d+\\.?\\d+)")
	private String location;
	
	private int mobile = Constants.MOBILE;
	
	public VirtualSensor(String sensorId) {
		this.sensorId = sensorId;
	}

	public VirtualSensor() {
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof VirtualSensor)) {
			return false;
		}
		VirtualSensor other = (VirtualSensor) obj;
		return sensorId.equals(other.sensorId);
	}

	public String getSensorId() {
		return sensorId;
	}

	public void setSensorId(String sensorId) {
		this.sensorId = sensorId;
	}

	public String getProviderId() {
		return providerId;
	}

	public void setProviderId(String providerId) {
		this.providerId = providerId;
	}

	public Type getType() {
		return type;
	}

	public void setType(Type type) {
		this.type = type;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public DataType getDataType() {
		return dataType;
	}

	public void setDataType(DataType dataType) {
		this.dataType = dataType;
	}

	public String getUnit() {
		return unit;
	}

	public void setUnit(String unit) {
		this.unit = unit;
	}

	public String getTokenAuth() {
		return tokenAuth;
	}

	public void setTokenAuth(String tokenAuth) {
		this.tokenAuth = tokenAuth;
	}

	public String getNumOfIterations() {
		return numOfIterations;
	}

	public void setNumOfIterations(String numOfIterations) {
		this.numOfIterations = numOfIterations;
	}

	public String getFreq() {
		return freq;
	}

	public void setFreq(String freq) {
		this.freq = freq;
	}

	public String getValue() {
		return value.trim();
	}

	public void setValue(String value) {
		this.value = value;
	}
	
	public int getMobile() {
		return mobile;
	}

	public boolean isMobileSensor() {
		return mobile == Constants.MOBILE;
	}

	public boolean isStaticSensor() {
		return mobile == Constants.STATIC;
	}

	public void setMobile(int mobile) {
		this.mobile = mobile;
	}

	public String getLocation() {
		return location;
	}

	public void setLocation(String location) {
		this.location = location;
	}

	public enum DataType {
		NUMBER, BOOLEAN, TEXT,
	}
	
	public enum Type {
		caudalimetre,contenidor,electrovalvula,etr,humitat,no2,o3,parking,pluvio,pm10,so2,temperatura,transit,vent		
	}
	
	public enum ComponentType {
		aparcament,caudalimetre,contenidor,electrovalvula,estaciometeo,gateway,generic,qualitataire,router,sonometre,transit
	}

	public String getComponentId() {
		return componentId;
	}

	public void setComponentId(String componentId) {
		this.componentId = componentId;
	}

	public ComponentType getComponentType() {
		return componentType;
	}

	public void setComponentType(ComponentType componentType) {
		this.componentType = componentType;
	}


}
