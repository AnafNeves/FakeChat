// Consent Form =================================================================

const ConsentForm = {
    type: jsPsychSurvey,
    survey_json: function () {
        // Get URL variables
        let urlvars = jsPsych.data.urlVariables()

        // Logo and title
        let text =
            "<img src='https://blogs.brighton.ac.uk/sussexwrites/files/2019/06/University-of-Sussex-logo-transparent.png' width='150px' align='right'/><br><br><br><br><br>" +
            "<h1>Informed Consent</h1>"

        // Main Text
        text +=
            // Overview
            "<h4>Invitation to Take Part</h4>" +
            "You are being invited to take part in a research study to further our understanding of perceptions of social interactions. " +
            "Thank you for carefully reading this information sheet. " +
            "This study is being conducted by student researchers Riehana Aziz, Emma Benn, Maisie Bennett, Summer-Lili Cloke and Poppy Duval-Johnston, and project supervisor Dr Dominique Makowski from the School of Psychology, University of Sussex (see contact information below). " +
            "This study needs to be completed on a computer/laptop to ensure all elements of the task appear correctly. If at any point you wish to stop participating and withdraw your data, please close the tab. This will prevent your data from being saved. Please be aware, your data will be anonymised and encrypted, meaning it will be impossible to withdraw your data upon <b>total completion and agreement for this to happen. </b>" +
            // Description
            "<h4>Why have I been invited to take part and what will I do?</h4>" +
            "This study aims to investigate perceptions of social interactions and how this relates to trust, loneliness, empathy, social effort, and social relationship quality. " +
            "In addition to this, the study investigates the relationship between interoceptive sensibility (bodily awareness - how aware you are of the processing and integration of your internal bodily states) and mental and physical health conditions. " +
            "To participate in this study, you will be asked to complete a series of questionnaires related to <b>mood, mental and physical health, social experiences, and attitudes.</b>" +
            "<p>Before the main task, you will respond to questions about:</p>" +
            // questionnaires before taks
            "<ul style='padding-left: 30px;'>" +
            "<li><b>Social and emotional experiences:</b> Feelings of social and emotional loneliness, and social isolation.</li>" +
            "<li><b>General well-being:</b> Life satisfaction, mood, and anxiety levels.</li>" +
            "<li><b>Mental health:</b> Presence of psychiatric disorders (conditions affecting mood, thinking, or behaviour) and any current treatments you may be following, ranging from mindfulness practices to medication.</li>" +
            "<li><b>Physical health:</b> Psychosomatic conditions caused by autonomic nervous system dysfunction, including musculoskeletal, dermatological, cardiovascular, and other related disorders.</li>" +
            "</ul>" +
            //Task description
            "You will then begin the main task. You will be shown several screenshots of conversations taken from Reddit (with the consent of users for their images to be used during the study). " +
            "Please imagine that you are participating in the interaction while reading the dialogue. After each conversation, you will be asked a couple of questions about your experience. " +
            //Questionaires after tasks
            "<p>After completing the main task, you will then be asked to complete additional questionnaires on:</b></p>" +
            "<ul style='padding-left: 30px;'>" +
            "<li><b>Attitudes toward AI:</b> Your general views on AI and beliefs about its current abilities in producing videos, images, and other outputs.</li>" +
            "<li><b>Social and emotional experiences:</b> Feelings of sympathy and compassion for others, and your ability to take others' perspectives.</li>" +
            "<li><b>Social behavior:</b> Your social effort and social conscientiousness.</li>" +
            "<li><b>Self-perception and internal awareness:</b> Your ability to perceive and understand information from internal bodily signals in a variety of contexts, including during sexual activity.</li>" +
            "</ul>" +
            "The whole experiment will take you <b style='color:#FE6237;'>~25 minutes</b> to complete. Please make you sure that you are <b>attentive and in a quiet environment</b>, and that you have time to complete it in one go.</p> " +
            //Note
            "<p><b style='color:red'>Note:</b> We understand that some of these questions may feel <b>personal or potentially distressing.</b> " +
            "However, your responses provide important information about the general participant population, which will help us answer our research questions. " +
            "Your participation is completely <b>voluntary</b>. You are free to <b style='color:#4B0FD6;'>close the webpage at any time</b> if you do not wish to continue. Your data will not be saved if you do so. " +
            "Please be assured that all information you provide will remain <b>completely anonymous</b>.</p>" +
            // Risks
            "<h4>Are there any risks or benefits to taking part?</h4>" +
            "<p><b style='color:red'>Content warning:</b> The conversations you will read contain content that some people may find distressing, including themes of <b>alcohol use</b>, <b>self-harm</b>, <b>suicidal thoughts</b> and <b>mental health struggles</b>. " +
            "As stated above, you can <b style='color:#4B0FD6;'>withdraw from the study at any time</b> by closing the tab (your data will <b>not</b> be saved).</p> " +
            // Results and personal information
            "<h4>What will happen to the results and my personal information?</h4>" +
            "The results of this research will be written into scientific reports for Psychology dissertations. In addition, this data will be used for other studies and the anonymised data will be made publicly available. We anticipate being able to provide a summary of our findings on request from 30/05/26 (<i style='color:DodgerBlue;'>D.Makowski@sussex.ac.uk</i>). Your anonymity will be ensured in the way described in the consent information below. <b>Please read this information carefully</b> and then, if you wish to take part, please acknowledge that you have fully understood this sheet, and that you consent to take part in the study as it is described here.</p>" +
            "<p align='left'><b>Consent</b><br></p>" +
            // Bullet points
            "<li align='left'>I understand that by signing below I am agreeing to take part in the University of Sussex research described here, and that I have read and understood this information sheet</li>" +
            "<li align='left'>I understand that my participation is entirely voluntary, that I can choose not to participate in part or all of the study, and that I can withdraw at any stage without having to give a reason and without being penalized in any way (e.g., if I am a student, my decision whether or not to take part will not affect my grades).</li>" +
            "<li align='left'>I understand that since the study is anonymous, it will be impossible to withdraw my data once I have completed it.</li>" +
            "<li align='left'>I understand that my personal data will be used for the purposes of this research study and will be handled in accordance with Data Protection legislation. I understand that the University's Privacy Notice provides further information on how the University uses personal data in its research.</li>" +
            "<li align='left'>I understand that my collected data will be stored in a de-identified way. De-identified data may be made publicly available through secured scientific online data repositories.</li>"

        // End
        text +=
            "<li align='left'>By participating, you agree to follow the instructions and provide honest answers. If you do not wish to participate or if you don't have the time, simply close your browser.</li></p>" +
            "<p align='left'><br><sub><sup>For further information about this research, or if you have any concerns, please contact Dr Dominique Makowski (<i style='color:DodgerBlue;'>D.Makowski@sussex.ac.uk</i>), Riehana Aziz (<i style='color:DodgerBlue;'>ra549@sussex.ac.uk</i>), and/or Emma Benn (<i style='color:DodgerBlue;'>eb672@sussex.ac.uk</i>). This research has been approved (ER/....) by the Sciences & Technology Cross-Schools Research Ethics Committee (C-REC) (<i style='color:DodgerBlue;'>crecscitec@sussex.ac.uk</i>). The University of Sussex has insurance in place to cover its legal liabilities in respect of this study.</sup></sub></p>"

        // Return Survey
        return {
            showQuestionNumbers: false,
            completeText: "I read, understood, and I consent",
            pages: [
                {
                    elements: [
                        {
                            type: "html",
                            name: "ConsentForm",
                            html: text,
                        },
                    ],
                },
            ],
        }
    },
}

var demographics_browser_info = {
    type: jsPsychBrowserCheck,
    data: {
        screen: "browser_info",
        date: new Date().toLocaleDateString("en-GB"),
        time: new Date().toLocaleTimeString("en-GB"),
    },
    on_finish: function (data) {
        data["participantID"] = participantID

        // Rename
        dat = jsPsych.data.get().filter({ screen: "browser_info" }).values()[0]
        data["screen_height"] = dat["height"]
        data["screen_width"] = dat["width"]

        // Add URL variables - ?sona_id=x&exp=1
        let urlvars = jsPsych.data.urlVariables()
        data["researcher"] = urlvars["exp"]
    },
}

// Demographic questions

var demographic_questions = {
    type: jsPsychSurvey,
    survey_json: {
        title: "About yourself",
        completeText: "Continue",
        pageNextText: "Next",
        pagePrevText: "Previous",
        goNextPageAutomatic: false,
        showQuestionNumbers: false,
        showProgressBar: "aboveHeader",
        pages: [
            {
                elements: [
                    {
                        title: "What is your gender?",
                        name: "Gender",
                        type: "radiogroup",
                        choices: ["Male", "Female", "Other"],
                        isRequired: true,
                        colCount: 0,
                    },
                    {
                        type: "text",
                        title: "Please enter your age (in years)",
                        name: "Age",
                        isRequired: true,
                        inputType: "number",
                        min: 18,
                        max: 100,
                        placeholder: "e.g., 21",
                    },
                ],
            },
            {
                elements: [
                    {
                        title: "What is your highest completed education level?",
                        name: "Education",
                        type: "radiogroup",
                        choices: [
                            {
                                value: "Doctorate",
                                text: "University (doctorate)",
                            },
                            {
                                value: "Master",
                                text: "University (master)", // "<sub><sup>or equivalent</sup></sub>",
                            },
                            {
                                value: "Bachelor",
                                text: "University (bachelor)", // "<sub><sup>or equivalent</sup></sub>",
                            },
                            {
                                value: "High school",
                                text: "High school / Secondary school (or 6th form college)",
                            },
                            {
                                value: "Elementary school",
                                text: "Elementary school / Primary school",
                            },
                        ],
                        showOtherItem: true,
                        otherText: "Other",
                        otherPlaceholder: "Please specify",
                        isRequired: true,
                        colCount: 1,
                    },
                    {
                        visibleIf:
                            "{Education} == 'High school' || {Education} == 'Master' || {Education} == 'Bachelor'",
                        title: "Are you currently a student?",
                        name: "Student",
                        type: "boolean",
                        swapOrder: true,
                        isRequired: true,
                    },
                ],
            },
            {
                elements: [
                    {
                        title: "How would you describe your ethnicity?",
                        name: "Ethnicity",
                        type: "radiogroup",
                        choices: [
                            "White",
                            "Black",
                            "Hispanic/Latino",
                            "Middle Eastern/North African",
                            "South Asian",
                            "East Asian",
                            "Southeast Asian",
                            "Mixed",
                            "Prefer not to say",
                        ],
                        showOtherItem: true,
                        otherText: "Other",
                        otherPlaceholder: "Please specify",
                        isRequired: false,
                        colCount: 1,
                    },
                ],
            },
        ],
    },
    data: {
        screen: "demographic_questions",
    },
}

// Feedback, Debrief, Thank you Screen
var experiment_feedback = {
    type: jsPsychSurvey,
    survey_json: {
        title: "Feedback",
        description:
            "It is the end of the experiment! Don't hesitate to leave us a feedback.",
        completeText: "Complete the experiment",
        showQuestionNumbers: false,
        pages: [
            {
                elements: [
                    {
                        type: "html",
                        name: "Feedback_Alert",
                        html: "<p><b style='color:red;'>Answers to these questions will help us to contextualize your answers</b></p>",
                    },
                    {
                        type: "rating",
                        name: "Feedback_Enjoyment",
                        title: "Did you enjoy doing this experiment?",
                        isRequired: false,
                        rateMin: 0,
                        rateMax: 4,
                        rateType: "stars",
                    },
                    {
                        type: "rating",
                        name: "Feedback_Quality",
                        title: "To what extent did you do the experiment carefully and thoroughly?",
                        description: "Please be honest!",
                        isRequired: false,
                        rateMin: 0,
                        rateMax: 4,
                    },
                    {
                        type: "comment",
                        name: "Feedback_Text",
                        title: "Anything else you would like to share with us?",
                        description:
                            "Please note that these comments might be shared publicly as part of the results of this study - avoid sharing personal information.",
                        isRequired: false,
                    },
                    {
                        type: "html",
                        name: "Study_Swap",
                        html: "<p><b>If you are a dissertation student, email your study link to Emma Benn (<i style='color:DodgerBlue;'>eb672@sussex.ac.uk</i>), and we will complete it.</b></p>",
                    },
                ],
            },
        ],
    },
    data: {
        screen: "experiment_feedback",
    },
}

var demographics_debriefing = {
    type: jsPsychSurvey,
    survey_json: {
        showQuestionNumbers: false,
        completeText: "Continue",
        pages: [
            {
                elements: [
                    {
                        type: "html",
                        name: "Debrief",
                        html:
                            "<img src='https://blogs.brighton.ac.uk/sussexwrites/files/2019/06/University-of-Sussex-logo-transparent.png' width='150px' align='right'/><br><br><br><br><br>" +
                            "<h2>Debriefing</h2>" +
                            "<p align='left'> The purpose of this study was to examine how the framing of an interaction partner as an AI versus human influences people's trust, empathy, and perceived social presence during online personal conversations. " +
                            "In this experiment, some conversations were framed as being human-AI interactions, and others as being between two humans. " +
                            "However, all conversations were <b>fictional</b> and <b>created</b> specifically for this study <b>using ChatGPT-5.2. </b>" +
                            "<p>We apologise for the necessary deception used in this study. Deception was essential to maintain validity of our experimental manipulation. " +
                            "We hope you understand the rationale behind this decision." +
                            "<p>We were also interested in how individual differences (such as loneliness, mental wellbeing, interoceptive awareness, AI familiarity and social motivation) relate to these reactions. Understanding these factors gives insight into why and how people may respond differently to online social partners, informing future research surrounding AI use.</p>" +
                            "<p align='left'><b>Thank you again!</b> Your participation in this study will be kept completely confidential. If you have any questions or concerns about the project, please contact Dr Dominique Makowski (<i style='color:DodgerBlue;'>D.Makowski@sussex.ac.uk</i>), Riehana Aziz (<i style='color:DodgerBlue;'>ra549@sussex.ac.uk</i>), and/or Emma Benn (<i style='color:DodgerBlue;'>eb672@sussex.ac.uk</i>).</p>" +
                            "<p align='center' style='color: black; font-size: 30px;'><b>To complete your participation in this study, click on 'Continue' and wait until your responses have been successfully saved before closing the tab. If you do not wish for your data to be saved, please close the tab now. Saving your data at this point will prevent you from being able to be withdraw your data from the study.</b></p>" +
                            //Resources

                            // "<p style='color: red; font-weight: bold;'>We appreciate that some of the content in this study can be heavy and distressing. If you or someone you know is struggling with their mental health, please do not hesitate to reach out to the following charities for further support.</p>" +
                            // "<p>If you or someone you know is in immediate danger or thinking about ending their life, please call 999 or go to your nearest A&E.</p>" +
                            // "<p style='color: red;'><b> 24/7 Crisis lines:</b> </p>" +
                            // "<p>Call The <b>Samaritans</b> on <b>116 123</b></p>" +
                            // "<>Text 'SHOUT' to 85258</p>" +
                            // "<p style='color: red;'><b>Support via charities:</b></p>" +
                            // "<p style='color: red;'>Call the Mind welfare line, open Monday - Friday (9 am to 5 pm): 0300 123 3393 or visit their website for tools and strategies</p>" +
                            // "<a style='color: red;' href='https://www.mind.org.uk/need-urgent-help/using-this-tool/'>www.mind.org.uk/need-urgent-help</a>" +
                            // "<p style='color: red;'>Mental Health Foundation: </p><a style='color: red;' href='https://www.mentalhealth.org.uk/'>www.mentalhealth.org.uk</a>" +
                            // "<p style='color: red;'><b>Local support:</b> Use the government website to find support services near you </p><a style='color: red;' href='https://www.nhs.uk/nhs-services/mental-health-services/'>www.nhs.uk/nhs-services/mental-health-services</a><br><br>",

                            "<p align='center' style='color: red; font-size: 32px;'><b>Support Resources</b></p>" +
                            "<p>We appreciate that some of the content in this study may be personal or distressing. " +
                            "Below are resources you can access for support related to mental health, psychiatric conditions, and wellbeing. " +
                            "Your participation is appreciated, and we encourage you to reach out if you need help.</p>" +
                            //Imediate crisis
                            "<p align='left' style='color: red; font-size: 16px;'><b>Immediate Crisis Support:</b></p>" +
                            " If you or someone you know is in <b>immediate danger or thinking about ending their life</b>, please call <b>999</b> or go to your nearest A&E.</li>" +
                            "<p><b>Mental Health Support:</b></p>" +
                            "<ul style='padding-left: 30px;'>" +
                            "<li>Call The Samaritans (24/7) on 116 123</li>" +
                            "<li>Text 'SHOUT' to 85258 for 24/7 text support</li>" +
                            "<li>Mind Welfare Line (Monday-Friday, 9am-5pm): 0300 123 3393 | <a href='https://www.mind.org.uk/need-urgent-help' target='_blank'>Visit website</a></li>" +
                            "<li>Mental Health Foundation: <a href='https://www.mentalhealth.org.uk' target='_blank'>www.mentalhealth.org.uk</a></li>" +
                            "<li>Local NHS mental health services: <a href='https://www.nhs.uk/nhs-services/mental-health-services' target='_blank'>Find local support</a></li>" +
                            "</ul>" +
                            // Neurodevelopment Conditions
                            "<p><b>Psychiatric and Neurodevelopmental Conditions:</b></p>" +
                            "<ul style='padding-left: 30px;'>" +
                            "<li><b>Addiction:</b> FRANK Drugs Helpline: 0300 123 6600 | <a href='https://www.talktofrank.com/' target='_blank'>Visit website</a></li>" +
                            "<li><b>ADHD:</b>" +
                            "<ul style='padding-left: 30px;'>" +
                            "<li>ADHD UK: <a href='https://adhduk.co.uk/' target='_blank'>www.adhduk.co.uk</a> | Support groups: <a href='https://adhduk.co.uk/support/' target='_blank'>https://adhduk.co.uk/support/</a></li>" +
                            "<li>ADHD Adult UK: <a href='https://www.adhdadult.uk/' target='_blank'>www.adhdadult.uk</a> | Parenting guide (PDF): <a href='https://www.adhdadult.uk/wp-content/uploads/2023/06/Parenting-children-with-ADHD-guide.pdf' target='_blank'>Download PDF</a></li>" +
                            "</ul>" +
                            "<li><b>Autism:</b> " +
                            "<ul style='padding-left: 30px;'>" +
                            "<li>National Autistic Society: <a href='https://www.autism.org.uk/' target='_blank'>www.autism.org.uk</a></li>" +
                            "<li>Ambitious about Autism: Call 020 8815 5444 | <a href='https://www.ambitiousaboutautism.org.uk' target='_blank'>Visit website</a></li>" +
                            "<li>Autism Central: <a href='https://www.autismcentral.org.uk/' target='_blank'>www.autismcentral.org.uk</a></li>" +
                            "</ul>" +
                            "</ul>" +
                            //Other Disorders
                            "<p><b>Disorder-specific Support:</b></p>" +
                            "<ul style='padding-left: 30px;'>" +
                            "<li>Bipolar Disorder: <a href='https://www.bipolaruk.org/' target='_blank'>Bipolar UK</a> | Mind support: <a href='https://www.mind.org.uk/' target='_blank'>Mind</a></li>" +
                            "<li>Borderline Personality Disorder (BPD): <a href='https://www.nhs.uk/mental-health/conditions/borderline-personality-disorder/' target='_blank'>NHS Info</a></li>" +
                            "<li>Major Depressive Disorder (MDD): <a href='https://www.mind.org.uk/information-support/types-of-mental-health-problems/depression/' target='_blank'>Mind: Depression</a></li>" +
                            "<li>Obsessive-Compulsive Disorder (OCD): <a href='https://www.ocduk.org/' target='_blank'>OCD-UK</a></li>" +
                            "<li>Panic Disorder: <a href='https://www.mind.org.uk/information-support/types-of-mental-health-problems/anxiety-and-panic-attacks' target='_blank'>Mind: Anxiety & Panic</a></li>" +
                            "<li>Post-Traumatic Stress Disorder (PTSD): <a href='https://www.mentalhealth.org.uk/explore-mental-health/a-z-topics/post-traumatic-stress-disorder-ptsd' target='_blank'>Mental Health Foundation</a></li>" +
                            "<li>Schizophrenia: <a href='https://www.nhs.uk/mental-health/conditions/schizophrenia/' target='_blank'>NHS Info</a></li>" +
                            "<li>Social Anxiety Disorder (Social Phobia): <a href='https://www.mind.org.uk/information-support/types-of-mental-health-problems/phobias/types-of-phobia/#WhatIsSocialPhobia' target='_blank'>Mind: Social Anxiety</a></li>" +
                            "<li>Specific Phobias: <a href='https://www.nhs.uk/mental-health/conditions/phobias/' target='_blank'>NHS Info</a></li>" +
                            "<li>Eating Disorders (e.g., Anorexia, Bulimia, Binge Eating): <a href='https://www.nhs.uk/mental-health/feelings-symptoms-behaviours/behaviours/eating-disorders/overview/' target='_blank'>NHS Info</a> | <a href='https://www.beateatingdisorders.org.uk/' target='_blank'>Beat Eating Disorders</a></li>" +
                            "</ul>" +
                            //Physical ans somatic Health
                            "<p align='left'><b>Support Resources for Physical and Somatic Health Conditions</b></p>" +
                            "<p>If any of the following conditions raised concerns for you, or if you would like more information and support, the following organisations and resources may be helpful.</p>" +
                            "<ul style='padding-left: 30px;'>" +
                            "<li><b>Musculoskeletal and Pain Conditions:</b><br>" +
                            "Hypermobility Syndrome (e.g., Ehlers-Danlos), Fibromyalgia, Chronic Fatigue Syndrome, Chronic Pain, Back Pain, Muscle Tension<br>" +
                            "<a href='https://www.versusarthritis.org/' target='_blank'>Versus Arthritis</a> - Advice and support for musculoskeletal conditions<br>" +
                            "<a href='https://www.fibromyalgia.org/' target='_blank'>Fibromyalgia Support UK</a> - UK-based resources and support groups<br>" +
                            "<a href='https://www.actionforme.org.uk/' target='_blank'>Action for ME</a> - Support for Myalgic Encephalomyelitis / Chronic Fatigue Syndrome" +
                            "</li>" +
                            "<li><b>Dermatological and Skin Conditions:</b><br>" +
                            "Skin rashes, Eczema, Psoriasis, Sjogren's Syndrome<br>" +
                            "<a href='https://www.nhsinform.scot/illnesses-and-conditions/skin-hair-and-nails/' target='_blank'>NHS: Skin Conditions</a><br>" +
                            "<a href='https://www.psoriasis.org/' target='_blank'>Psoriasis Association</a> - Information and support" +
                            "</li>" +
                            "<li><b>Cardiovascular Conditions:</b><br>" +
                            "Chest Pain, Cardiac Arrhythmia, Hypertension, Hypotension<br>" +
                            "<a href='https://www.bhf.org.uk/' target='_blank'>British Heart Foundation</a> - Advice, support, and local services" +
                            "</li>" +
                            "<li><b>Gastrointestinal Conditions:</b><br>" +
                            "Irritable Bowel Syndrome (IBS), GERD, Crohn's Disease, Ulcerative Colitis, Celiac Disease, Gluten or Lactose Intolerance<br>" +
                            "<a href='https://www.crohnsandcolitis.org.uk/' target='_blank'>Crohn's & Colitis UK</a> - Support and information<br>" +
                            "<a href='https://www.coeliac.org.uk/' target='_blank'>Coeliac UK</a> - Advice and resources" +
                            "</li>" +
                            "<li><b>Respiratory Conditions:</b><br>" +
                            "Shortness of Breath, Asthma, COPD, Sleep Apnea, Chronic Bronchitis<br>" +
                            "<a href='https://www.asthma.org.uk/' target='_blank'>Asthma UK</a> - Advice and support<br>" +
                            "<a href='https://www.blf.org.uk/' target='_blank'>British Lung Foundation</a> - Resources for COPD and lung health" +
                            "</li>" +
                            "<li><b>Neurological Conditions:</b><br>" +
                            "Nausea/Vomiting, Dizziness, Migraine, Neuropathy, Epilepsy, Multiple Sclerosis (MS)<br>" +
                            "<a href='https://www.epilepsy.org.uk/' target='_blank'>Epilepsy Society</a> - Information, advice, and support groups<br>" +
                            "<a href='https://www.mssociety.org.uk/' target='_blank'>MS Society UK</a> - Support for Multiple Sclerosis<br>" +
                            "<a href='https://www.migrainetrust.org/' target='_blank'>Migraine Trust</a> - Resources and patient support" +
                            "</li>" +
                            "<li><b>Genitourinary Conditions:</b><br>" +
                            "Frequent Urination, Endometriosis, Interstitial Cystitis, Chronic Pelvic Pain Syndrome<br>" +
                            "<a href='https://www.endometriosis-uk.org/' target='_blank'>Endometriosis UK</a> - Information and support<br>" +
                            "<a href='https://www.ichelp.org/' target='_blank'>Interstitial Cystitis Support Network UK</a> - Guidance and resources" +
                            "</li>" +
                            "</ul>" +
                            "<p>We encourage you to <b>reach out to any of the above services</b> if you feel <b>you need support or advice.</b> Your wellbeing is important, and there are resources available to help at any time.</p>",
                    },
                ],
            },
        ],
    },
    data: {
        screen: "demographics_debrief",
    },
    // on_finish: function (data) {
    //     let score = check_attentionchecks()
    //     if (score >= 0.75) {
    //         data["AttentionChecks"] = "Pass"
    //         data["AttentionScore"] = score
    //     } else {
    //         data["AttentionChecks"] = "Fail"
    //         data["AttentionScore"] = score
    //     }
    // },
}

var demographics_endscreen = {
    type: jsPsychSurvey,
    survey_json: function () {
        text =
            "<h2 style='color:green;'>Data saved successfully!</h2>" +
            "<p>Thank you for participating, it means a lot to us.</p>"

        // Snowball
        // text +=
        //     "<p>Don't hesitate to share the study by sending this link <i>(but please don't reveal the details of the experiment)</i>:</p>" +
        //     "<p><a href='" +
        //     "..." +
        //     "'>" +
        //     "..." +
        //     "<a/></p>"

        // Return survey
        return {
            showQuestionNumbers: false,
            completeText: "Finish",
            pages: [
                {
                    elements: [
                        {
                            type: "html",
                            name: "Endscreen",
                            html: text,
                        },
                    ],
                },
            ],
        }
    },
    data: {
        screen: "demographics_endscreen",
    },
}
