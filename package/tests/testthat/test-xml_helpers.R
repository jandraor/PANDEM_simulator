
test_var_xml <- xml2::read_xml('
  <root>
    <doc1 xmlns = "http://docs.oasis-open.org/xmile/ns/XMILE/v1.0">
	<dimensions>
		<dim name="Age">
			<elem name="Young"/>
			<elem name="Adult"/>
		</dim>
		<dim name="Age_contactee">
			<elem name="Young"/>
			<elem name="Adult"/>
		</dim>
		<dim name="Age_contactor">
			<elem name="Young"/>
			<elem name="Adult"/>
		</dim>
		<dim name="Region">
			<elem name="NL13"/>
			<elem name="NL21"/>
			<elem name="NL22"/>
		</dim>
	</dimensions>
	    <model name="Inputs_Epi">
	      <variables>
			    <aux name="ClinicalFraction" access="output">
			      <doc>The fraction of people who develop symptoms, and so are detected by the surveillance system</doc>
					  <dimensions>
					    <dim name="Age"/>
				    </dimensions>
				    <eqn>.5</eqn>
				  </aux>
			    <aux name="Pathogen Seed Time" access="output">
				    <dimensions>
					    <dim name="Region"/>
					    <dim name="Age"/>
				    </dimensions>
				    <eqn>0</eqn>
			    </aux>
			    <aux name="Infectivity" access="output">
				    <eqn>0.1</eqn>
			    </aux>
			    <aux name="BaseContacts" access="output">
			      <dimensions>
			        <dim name="Age_contactor"/>
					    <dim name="Age_contactee"/>
				    </dimensions>
				    <element subscript="Young, Young">
				      <eqn>1</eqn>
				    </element>
				    <element subscript="Young, Adult">
					    <eqn>2</eqn>
				    </element>
				    <element subscript="Adult, Young">
					    <eqn>3</eqn>
				    </element>
				    <element subscript="Adult, Adult">
					    <eqn>4</eqn>
				    </element>
			    </aux>
		    </variables>
		  </model>
    </doc1>
  </root>')

test_that("format_par_obj returns the expected list for array with a common value", {

  module_xml  <- xml2::xml_find_first(test_var_xml, ".//d1:model")
  module_name <- xml2::xml_attr(module_xml, "name")

  dims_obj    <- create_dims_obj(test_var_xml)

  vars_xml   <- xml2::xml_find_first(module_xml, ".//d1:variables")
  aux_obj    <- xml2::xml_find_first(vars_xml, ".//d1:aux")

  actual   <- format_par_obj(aux_obj, module_name, dims_obj, "input")

  expected <- list(
    name           = "ClinicalFraction",
    category       = module_name,
    description    = "The fraction of people who develop symptoms, and so are detected by the surveillance system",
    array          = TRUE,
    dimensions     = list(Age = c("Young", "Adult")),
    default_values = list(`Inputs_Epi.ClinicalFraction[Young]` = 0.5,
                          `Inputs_Epi.ClinicalFraction[Adult]` = 0.5))

  expect_equal(actual, expected)
})

test_that("format_par_obj returns the expected list for array with a common value with two or more dimensions", {

  module_xml  <- xml2::xml_find_first(test_var_xml, ".//d1:model")
  module_name <- xml2::xml_attr(module_xml, "name")

  dims_obj    <- create_dims_obj(test_var_xml)

  vars_xml   <- xml2::xml_find_first(module_xml, ".//d1:variables")
  aux_obj    <- xml2::xml_find_all(vars_xml, ".//d1:aux")[[2]]

  actual   <- format_par_obj(aux_obj, module_name, dims_obj, "input")

  expected <- list(
    name           = "Pathogen_Seed_Time",
    category       = module_name,
    description    = "NA",
    array          = TRUE,
    dimensions     = list(Region = c("NL13", "NL21", "NL22"),
                          Age    = c("Young", "Adult")),
    default_values = list(`Inputs_Epi.Pathogen_Seed_Time[NL13,Young]` = 0,
                          `Inputs_Epi.Pathogen_Seed_Time[NL21,Young]` = 0,
                          `Inputs_Epi.Pathogen_Seed_Time[NL22,Young]` = 0,
                          `Inputs_Epi.Pathogen_Seed_Time[NL13,Adult]` = 0,
                          `Inputs_Epi.Pathogen_Seed_Time[NL21,Adult]` = 0,
                          `Inputs_Epi.Pathogen_Seed_Time[NL22,Adult]` = 0))

  expect_equal(actual, expected)
})

test_that("format_par_obj returns the expected list for a multidimensional array with specific values", {

  module_xml  <- xml2::xml_find_first(test_var_xml, ".//d1:model")
  module_name <- xml2::xml_attr(module_xml, "name")

  dims_obj    <- create_dims_obj(test_var_xml)

  vars_xml   <- xml2::xml_find_first(module_xml, ".//d1:variables")
  aux_obj    <- xml2::xml_find_all(vars_xml, ".//d1:aux")[[4]]

  actual   <- format_par_obj(aux_obj, module_name, dims_obj, "input")

  expected <- list(
    name           = "BaseContacts",
    category       = module_name,
    description    = "NA",
    array          = TRUE,
    dimensions     = list(Age_contactor = c("Young", "Adult"),
                          Age_contactee = c("Young", "Adult")),
    default_values = list(`Inputs_Epi.BaseContacts[Young,Young]` = 1,
                          `Inputs_Epi.BaseContacts[Young,Adult]` = 2,
                          `Inputs_Epi.BaseContacts[Adult,Young]` = 3,
                          `Inputs_Epi.BaseContacts[Adult,Adult]` = 4))

  expect_equal(actual, expected)
})
